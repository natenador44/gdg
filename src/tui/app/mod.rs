use std::{
    collections::HashMap,
    path::PathBuf,
    sync::mpsc::{Receiver, channel},
    thread,
    time::Duration,
};

use anyhow::Result;
use ratatui::{
    Frame,
    crossterm::event::{self, Event, KeyCode, KeyEventKind},
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Style},
    widgets::{Block, Borders, Paragraph},
};
use tracing::{debug, info};
use tui_tree_widget::{Tree, TreeItem, TreeState};

use crate::{
    graph::{
        Dependency, DependencyGraph, DependencyId, DependencyStatus, Identifier, Node, NodeIdx,
        Relationship, TraversableGraph, create_graph,
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
                    state.view_state.key_down();
                }
            }
            Message::SelectPrev => {
                if let AppState::Running(state) = &mut self.app_state {
                    state.view_state.key_up();
                }
            }
            Message::ToggleOpenOnSelected => {
                if let AppState::Running(state) = &mut self.app_state {
                    state.view_state.toggle_selected();
                }
            }
            Message::GoToSubtree => {
                if let AppState::Running(state) = &mut self.app_state {
                    let selected_idx = state.view_state.selected()[0];
                    let node = state
                        .graph
                        .children_with_relationship(selected_idx, Relationship::SubtreeFoundHere)
                        .next()
                        .expect("subtree relationship found");

                    state.view_state.select(vec![node.idx]);
                    state.view_state.scroll_selected_into_view();
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
            AppState::Running(state) => render_dependency_graph_view(frame, state),
            AppState::Error(e) => frame.render_widget(
                Paragraph::new(format!("an error occurred: {e}")),
                frame.area(),
            ),
            AppState::Done => {}
        }
    }

    pub fn handle_event(&self) -> Result<Option<Message>> {
        if event::poll(Duration::from_millis(150))? {
            match event::read()? {
                Event::Key(key) if key.kind == KeyEventKind::Press => match key.code {
                    KeyCode::Char('q') => Ok(Some(Message::Quit)),
                    KeyCode::Char('j') | KeyCode::Down => Ok(Some(Message::SelectNext)),
                    KeyCode::Char('k') | KeyCode::Up => Ok(Some(Message::SelectPrev)),
                    KeyCode::Char(' ') | KeyCode::Enter => Ok(Some(Message::ToggleOpenOnSelected)),
                    KeyCode::Char('s') => Ok(Some(Message::GoToSubtree)),
                    _ => Ok(None),
                },
                _ => Ok(None),
            }
        } else {
            Ok(None)
        }
    }
}

fn render_dependency_graph_view(frame: &mut Frame<'_>, state: &mut RunningState) {
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
        .constraints([Constraint::Percentage(40), Constraint::Percentage(60)])
        .split(stage[0]);

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

    if state.view_state.selected().is_empty() {
        if !state.view_state.select_first() {
            debug!("first not selected");
        }
    }

    frame.render_stateful_widget(tree, main[0], &mut state.view_state);
    frame.render_widget(
        Paragraph::new("Dep details go here")
            .block(Block::new().borders(Borders::all()).title("Details")),
        main[1],
    );

    render_context_menu(frame, state, stage[1]);
}

fn render_context_menu(frame: &mut Frame<'_>, state: &mut RunningState, area: Rect) {
    let mut context_menu_text = String::from("j/↓ - select next, k/↑ - select previous");

    if !state.view_state.selected().is_empty() {
        let graph = &state.graph;
        let selected = state.view_state.selected()[state.view_state.selected().len() - 1];

        if graph.node_has_dependencies(selected) {
            context_menu_text += ", space/⏎ - toggle open on selected"
        }

        let node = graph.get_node(selected);

        if node
            .dependency
            .statuses
            .contains(&DependencyStatus::SubTreeAlreadyListed)
        {
            context_menu_text += ", s - go to sub tree"
        }
    }

    frame.render_widget(
        Paragraph::new(context_menu_text).block(Block::bordered().title("Context Menu")),
        area,
    );
}

fn build_tree_item_for_parent<'a>(graph: &TraversableGraph<'a>) -> Vec<TreeItem<'a, NodeIdx>> {
    let mut children = Vec::new();
    for c in graph.nodes_with_relationship(Relationship::DependsOn) {
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
}

enum LoadState {
    Success(DependencyGraph),
    Fail(anyhow::Error),
}

struct RunningState {
    graph: DependencyGraph,
    view_state: TreeState<NodeIdx>,
    selected_dep: Option<NodeIdx>,
    search_text: Option<String>,
}

impl RunningState {
    fn new(graph: DependencyGraph) -> Self {
        Self {
            graph,
            view_state: TreeState::default(),
            selected_dep: None,
            search_text: None,
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
