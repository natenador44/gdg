use std::{
    path::PathBuf,
    sync::mpsc::{Receiver, channel},
    thread,
    time::Duration,
};

use anyhow::Result;
use itertools::Itertools;
use ratatui::{
    Frame,
    crossterm::event::{self, Event, KeyCode, KeyEventKind, KeyModifiers},
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Style},
    text::Line,
    widgets::{Block, Borders, Paragraph},
};
use tracing::debug;
use tui_textarea::TextArea;
use tui_tree_widget::{Tree, TreeItem, TreeState};

use crate::{
    graph::{
        DependencyGraph, DependencyId, Identifier, NodeIdx, Relationship, TraversableGraph,
        create_graph,
    },
    input,
};

pub struct App {
    app_state: AppState,
}

impl App {
    pub fn new() -> Self {
        Self {
            app_state: AppState::default(),
        }
    }

    pub fn is_done(&self) -> bool {
        matches!(&self.app_state, AppState::Done)
    }

    pub fn update(&mut self, message: Message) {
        match message {
            Message::Quit => self.app_state = AppState::Done,
            Message::Load(path_buf, root_name) => {
                self.app_state = load_in_background(path_buf, root_name);
            }
            Message::SelectNext => {
                if let AppState::Running(state) = &mut self.app_state {
                    state.tree_state.key_down();
                }
            }
            Message::SelectPrev => {
                if let AppState::Running(state) = &mut self.app_state {
                    state.tree_state.key_up();
                }
            }
            Message::ToggleOpenOnSelected => {
                if let AppState::Running(state) = &mut self.app_state {
                    state.tree_state.toggle_selected();
                }
            }
            Message::GoToSubtree => {
                if let AppState::Running(state) = &mut self.app_state {
                    let selected_idx =
                        state.tree_state.selected()[state.tree_state.selected().len() - 1];
                    let node = state
                        .graph
                        .adj_nodes_with_relationship(selected_idx, Relationship::SubtreeFoundHere)
                        .next()
                        .expect("subtree relationship found");

                    let mut cur_node_idx = node.idx;
                    let mut tree_id = vec![node.idx];
                    while let Some(node) = state
                        .graph
                        .adj_nodes_with_relationship(cur_node_idx, Relationship::Dependent)
                        .next()
                    {
                        if node.idx.is_root() {
                            // the tree doesn't account for root
                            break;
                        }
                        tree_id.push(node.idx);
                        cur_node_idx = node.idx;
                    }
                    tree_id.reverse();

                    for i in 0..tree_id.len() - 1 {
                        state.tree_state.open(tree_id[0..=i].to_vec());
                    }

                    state.tree_state.select(tree_id);
                    state.tree_state.scroll_selected_into_view();
                }
            }
            Message::PageDown => {
                if let AppState::Running(state) = &mut self.app_state {
                    state
                        .tree_state
                        .select_relative(|mut current| current.get_or_insert(0).saturating_add(25));
                }
            }
            Message::PageUp => {
                if let AppState::Running(state) = &mut self.app_state {
                    state
                        .tree_state
                        .select_relative(|mut current| current.get_or_insert(0).saturating_sub(25));
                }
            }
            Message::FocusSearch => {
                if let AppState::Running(state) = &mut self.app_state {
                    state.focus = Focus::Search;
                }
            }
            Message::FocusTree => {
                if let AppState::Running(state) = &mut self.app_state {
                    state.focus = Focus::Tree;
                }
            }
            Message::SelectAllSearchText => {
                if let AppState::Running(state) = &mut self.app_state {
                    state.search_area.select_all();
                }
            }
        }
    }

    pub fn view(&mut self, frame: &mut Frame) {
        match &mut self.app_state {
            AppState::Init => frame.render_widget(Paragraph::new("Welcome to GDG!"), frame.area()),
            AppState::Loading(receiver) => {
                if let Ok(msg) = receiver.try_recv() {
                    self.app_state = match msg {
                        LoadState::Success(dependency_graph) => {
                            AppState::Running(RunningState::new(dependency_graph))
                        }
                        LoadState::Fail(error) => AppState::Error(error),
                    };
                } else {
                    frame.render_widget(Paragraph::new("Loading dependency graph.."), frame.area());
                }
            }
            AppState::Running(state) => render_main_view(frame, state),
            AppState::Error(e) => frame.render_widget(
                Paragraph::new(format!("an error occurred: {e}")),
                frame.area(),
            ),
            AppState::Done => {}
        }
    }

    pub fn handle_event(&mut self) -> Result<Option<Message>> {
        if event::poll(Duration::from_millis(150))? {
            let evt = event::read()?;
            if search_focused(&self.app_state) {
                if let Event::Key(key) = &evt
                    && key.kind == KeyEventKind::Press
                {
                    match key.code {
                        KeyCode::Esc => return Ok(Some(Message::FocusTree)),
                        KeyCode::Char('a') if key.modifiers.contains(KeyModifiers::CONTROL) => {
                            return Ok(Some(Message::SelectAllSearchText));
                        }
                        _ => {
                            if let AppState::Running(state) = &mut self.app_state {
                                if state.focus == Focus::Search {
                                    state.search_area.input(evt);
                                }
                            }
                        }
                    }
                } else if let AppState::Running(state) = &mut self.app_state {
                    if state.focus == Focus::Search {
                        state.search_area.input(evt);
                    }
                }
                Ok(None)
            } else if dep_tree_focused(&self.app_state) {
                match &evt {
                    Event::Key(key) if key.kind == KeyEventKind::Press => match key.code {
                        KeyCode::Char('q') => Ok(Some(Message::Quit)),
                        KeyCode::Char('j') | KeyCode::Down => Ok(Some(Message::SelectNext)),
                        KeyCode::Char('k') | KeyCode::Up => Ok(Some(Message::SelectPrev)),
                        KeyCode::Char(' ') | KeyCode::Enter => {
                            Ok(Some(Message::ToggleOpenOnSelected))
                        }
                        KeyCode::Char('s') => Ok(Some(Message::GoToSubtree)),
                        KeyCode::Char('d') if key.modifiers.contains(KeyModifiers::CONTROL) => {
                            Ok(Some(Message::PageDown))
                        }
                        KeyCode::PageDown => Ok(Some(Message::PageDown)),
                        KeyCode::Char('u') if key.modifiers.contains(KeyModifiers::CONTROL) => {
                            Ok(Some(Message::PageUp))
                        }
                        KeyCode::PageUp => Ok(Some(Message::PageUp)),
                        KeyCode::Char('/') => Ok(Some(Message::FocusSearch)),
                        KeyCode::Char('f') if key.modifiers.contains(KeyModifiers::CONTROL) => {
                            Ok(Some(Message::FocusSearch))
                        }
                        _ => Ok(None),
                    },
                    _ => Ok(None),
                }
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }
}

fn dep_tree_focused(app_state: &AppState) -> bool {
    matches!(app_state, AppState::Running(state) if state.focus == Focus::Tree)
}

fn search_focused(app_state: &AppState) -> bool {
    matches!(app_state, AppState::Running(state) if state.focus == Focus::Search)
}

fn render_main_view(frame: &mut Frame<'_>, state: &mut RunningState) {
    /*
    * --- title ------------------------------------
    * | [ search ] |   selected dep details | help |
    * | > dep1     |                        | menu |
    * |   > dep2   |                        |      |
    * |     ...    |                        |      |
    * -- context menu ------------------------------

    context menu changes based on the dependency selected, if any
    */

    let stage = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Fill(1), Constraint::Length(3)])
        .split(frame.area());

    // skipping the help menu for now since that only shows up conditionally
    let main = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(45), Constraint::Percentage(55)])
        .split(stage[0]);

    render_dependency_graph_view(frame, state, main[0]);
    render_details_view(frame, state, main[1]);

    render_context_menu(frame, state, stage[1]);
}

fn render_details_view(frame: &mut Frame<'_>, state: &mut RunningState, area: Rect) {
    let details = if state.tree_state.selected().is_empty() {
        vec![Line::from("Select a dependency to view its details")]
    } else {
        let selected_idx = state.tree_state.selected()[state.tree_state.selected().len() - 1];
        let relationships = state
            .graph
            .adj_node_relationships(selected_idx)
            .map(|r| Line::from(format!("{r:?}")))
            .collect_vec();

        let mut all_details = vec![Line::from(format!("Selected Index: {selected_idx:?}"))];
        all_details.extend(relationships);
        all_details.push(Line::from(format!(
            "Tree ID: {:?}",
            state
                .tree_state
                .selected()
                .into_iter()
                .map(|i| i.to_string())
                .join(",")
        )));

        if let Some(subtree_node) = state
            .graph
            .adj_nodes_with_relationship(
                state.tree_state.selected()[state.tree_state.selected().len() - 1],
                Relationship::SubtreeFoundHere,
            )
            .next()
        {
            let mut subtree_id = vec![subtree_node.idx];
            let mut cur_idx = subtree_node.idx;
            while let Some(node) = state
                .graph
                .adj_nodes_with_relationship(cur_idx, Relationship::Dependent)
                .next()
            {
                if node.idx.is_root() {
                    break;
                }
                subtree_id.push(node.idx);
                cur_idx = node.idx;
            }

            subtree_id.reverse();

            all_details.push(Line::from(format!(
                "Subtree Tree ID: {}",
                subtree_id.iter().map(|i| i.to_string()).join(",")
            )));
        }

        all_details
    };

    frame.render_widget(
        Paragraph::new(details).block(Block::new().borders(Borders::all()).title("Details")),
        area,
    );
}

fn render_dependency_graph_view(frame: &mut Frame, state: &mut RunningState, area: Rect) {
    let split = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(3),
            Constraint::Length(3),
            Constraint::Fill(1),
        ])
        .split(area);

    if state.focus == Focus::Tree {
        // hide the cursor
        state
            .search_area
            .set_cursor_style(Style::default().bg(Color::default()));

        if state.tree_state.selected().is_empty() {
            state.tree_state.select_first();
        }
    } else {
        state
            .search_area
            .set_cursor_style(Style::default().bg(Color::White));
        state.tree_state.select(Vec::new());
    }

    state
        .search_area
        .set_block(Block::bordered().title("Search"));

    frame.render_widget(&state.search_area, split[0]);

    let mut path_display = state
        .tree_state
        .selected()
        .iter()
        .map(|i| state.graph.get_node(*i))
        .map(|n| match &n.dependency.id {
            DependencyId::Project(n) => format!("{n}(p)"),
            DependencyId::Artifact(artifact) => format!("{}(a)", artifact.name),
        })
        .join(" -> ");

    let view_size = area.as_size().width as usize - 4;
    if path_display.chars().count() > view_size {
        path_display = format!("..{}", &path_display[path_display.len() - view_size..])
    }

    frame.render_widget(
        Paragraph::new(path_display)
            .block(Block::bordered().title("Selected Path (a = artifact, p = project)")),
        split[1],
    );

    let tg = state.graph.traversable_from_root();
    let roots = build_tree_item_for_parent(&tg);

    let tree = Tree::new(&roots)
        .expect("all identifiers are unique")
        .highlight_style(Style::new().bg(Color::Blue))
        .block(
            Block::new()
                .borders(Borders::all())
                .title(state.graph.root_name()),
        );

    frame.render_stateful_widget(tree, split[2], &mut state.tree_state);
}

fn render_context_menu(frame: &mut Frame<'_>, state: &mut RunningState, area: Rect) {
    let menu = match state.focus {
        Focus::Search => String::from("Esc - focus depenency tree, ctrl-a - select all"),
        Focus::Tree => {
            let mut context_menu_text = String::from(
                "q - quit, /|ctrl-f - search, j|↓ - select next, k|↑ - select previous",
            );

            if !state.tree_state.selected().is_empty() {
                let graph = &state.graph;
                let selected = state.tree_state.selected()[state.tree_state.selected().len() - 1];

                if graph.node_has_dependencies(selected) {
                    context_menu_text += ", space|⏎ - toggle open on selected"
                }

                if state
                    .graph
                    .adj_nodes_with_relationship(
                        state.tree_state.selected()[state.tree_state.selected().len() - 1],
                        Relationship::SubtreeFoundHere,
                    )
                    .next()
                    .is_some()
                {
                    context_menu_text += ", s - go to sub tree"
                }
            }
            context_menu_text
        }
    };

    frame.render_widget(
        Paragraph::new(menu).block(Block::bordered().title("Context Menu")),
        area,
    );
}

fn build_tree_item_for_parent<'a>(graph: &TraversableGraph<'a>) -> Vec<TreeItem<'a, NodeIdx>> {
    let mut children = Vec::new();
    for c in graph.nodes_with_relationship(Relationship::Dependency) {
        let item = TreeItem::new(
            c.idx,
            c.dependency.id.to_string(),
            build_tree_item_for_parent(&graph.traverse_to_node(c.idx)),
        )
        .expect("tree item created successfully");

        children.push(item);
    }

    children
}

fn load_in_background(dep_file: PathBuf, root_name: String) -> AppState {
    let (tx, rx) = channel();
    thread::spawn(move || {
        let parser = match input::get_as_parser(Some(dep_file)) {
            Ok(p) => p,
            Err(e) => {
                let _ = tx.send(LoadState::Fail(e));
                return;
            }
        };

        let result = create_graph(parser, Identifier::from(root_name));

        let msg = match result {
            Ok(g) => LoadState::Success(g),
            Err(e) => LoadState::Fail(e),
        };

        let _ = tx.send(msg);
    });

    AppState::Loading(rx)
}

pub enum Message {
    Quit,
    Load(PathBuf, String),
    SelectNext,
    SelectPrev,
    ToggleOpenOnSelected,
    GoToSubtree,
    PageDown,
    PageUp,
    FocusSearch,
    FocusTree,
    SelectAllSearchText,
}

enum LoadState {
    Success(DependencyGraph),
    Fail(anyhow::Error),
}

struct RunningState {
    graph: DependencyGraph,
    tree_state: TreeState<NodeIdx>,
    search_area: TextArea<'static>,
    focus: Focus,
}

#[derive(Default, PartialEq, Eq, Clone, Copy, Debug)]
enum Focus {
    Search,
    #[default]
    Tree,
}

impl RunningState {
    fn new(graph: DependencyGraph) -> Self {
        Self {
            graph,
            tree_state: TreeState::default(),
            search_area: TextArea::default(),
            focus: Focus::default(),
        }
    }
}

#[derive(Default)]
enum AppState {
    #[default]
    Init,
    Loading(Receiver<LoadState>),
    Running(RunningState),
    Error(anyhow::Error),
    Done,
}
