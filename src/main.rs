use std::{collections::HashMap, ffi::OsString, fs::read_to_string, io::stderr, path::PathBuf};

use anyhow::Result;
use clap::{CommandFactory, Parser};
use fnscript::{syn::Script, tokenizer::tokenize};

#[derive(Parser)]
#[clap(disable_help_flag = true)]
struct Options {
    /// The script to execute
    #[arg(allow_hyphen_values = true)]
    script: PathBuf,
    /// The arguments for the script
    #[arg(allow_hyphen_values = true)]
    args: Vec<OsString>,
}

fn main() -> Result<()> {
    // let mut args = std::env::args().skip(1);
    // // TODO construct error messages with clap
    // let file = args.next().ok_or_else(|| anyhow!("Needs a script"))?;
    let Options { script: file, args } = Options::parse();
    match file.to_string_lossy().as_ref() {
        "-h" => {
            Options::command().write_help(&mut stderr())?;
            return Ok(());
        }
        "--help" => {
            Options::command().write_long_help(&mut stderr())?;
            return Ok(());
        }
        _ => {}
    }

    let script = &read_to_string(&file)?;
    let tokens = tokenize(script);
    let script: Script = tokens.to_parse().parse()?;

    let mut command = script.to_command(
        file.file_name()
            .expect("Only files can be read")
            .to_string_lossy()
            .to_string(),
    )?;
    let matches = match command.try_get_matches_from_mut(&args) {
        Ok(matches) => matches,
        Err(_) => {
            script.print_help(command, &args)?;
            unreachable!()
        }
    };
    let (subcommand, matches) = matches
        .subcommand()
        .expect("Subcommands are currently required");

    let mut globals = HashMap::new();
    // TODO get from matches
    let mut args = HashMap::new();

    for fun in script.get_pre_fns(subcommand) {
        fun.exec(&mut globals, &mut args)?;
    }

    Ok(())
}
