use std::path::PathBuf;

use clap::{Arg, ArgAction, Command, value_parser};

pub struct Args {
    pub root_name: String,
    pub dependency_file: Option<PathBuf>,
    pub verbose: bool,
}

impl Args {
    pub fn parse() -> Self {
        let matches = Command::new("gdg")
            .arg(root_name_arg())
            .arg(file_arg())
            .arg(verbose_arg())
            .get_matches();

        Args {
            root_name: matches
                .get_one("root_name")
                .cloned()
                .expect("root_name is required"),
            dependency_file: matches.get_one::<PathBuf>("file").map(|f| f.to_path_buf()),
            verbose: matches.get_flag("verbose"),
        }
    }
}

fn root_name_arg() -> Arg {
    Arg::new("root_name")
        .required(true)
        .help("The text used to display the root node of the dependency graph")
}

fn file_arg() -> Arg {
    Arg::new("file")
        .short('f')
        .long("file")
        .value_parser(value_parser!(PathBuf))
}

fn verbose_arg() -> Arg {
    Arg::new("verbose")
        .short('v')
        .long("verbose")
        .action(ArgAction::SetTrue)
        .help("Enable stdout logging output for gdg. Can specify log level via the `GDG_LOG` environment variable (GDG_LOG=debug|info|warn|error). If not specified, the log level defaults to 'error'")
}
