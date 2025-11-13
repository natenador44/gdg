use std::path::PathBuf;

use crate::tui::app::{App, Message};
use anyhow::Result;
use ratatui::DefaultTerminal;

mod app;

pub fn run(root_name: String, dep_file: PathBuf) -> Result<()> {
    let mut terminal = ratatui::init();

    let result = do_run(&mut terminal, root_name, dep_file);

    ratatui::restore();

    result
}

fn do_run(terminal: &mut DefaultTerminal, root_name: String, dep_file: PathBuf) -> Result<()> {
    let mut app = App::new();

    app.update(Message::Load(dep_file, root_name));

    while !app.is_done() {
        terminal.draw(|f| app.view(f))?;

        let current_msg = app.handle_event()?;

        if let Some(msg) = current_msg {
            app.update(msg);
        }
    }
    Ok(())
}
