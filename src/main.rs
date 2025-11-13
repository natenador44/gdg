use crate::{args::Args, graph::create_graph};
use anyhow::Result;
use tracing_subscriber::{EnvFilter, fmt, layer::SubscriberExt, util::SubscriberInitExt};

mod args;
mod graph;
mod input;
mod parser;
mod tokens;
mod tui;

fn main() -> Result<()> {
    let args = Args::parse();

    if args.verbose {
        setup_logging();
    }

    // let parser = input::get_as_parser(args.dependency_file)?;

    // let g = create_graph(parser, args.root_name)?;
    // g.write_to_file("/home/nkroll/tmp/test.dot")?;

    tui::run(
        args.root_name,
        args.dependency_file.expect("specified dep file"),
    )?;

    Ok(())
}

fn setup_logging() {
    tracing_subscriber::registry()
        .with(fmt::layer())
        .with(EnvFilter::from_env("GDG_LOG"))
        .init();
}
