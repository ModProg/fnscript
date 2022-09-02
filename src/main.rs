use std::{
    ffi::OsString,
    fs::{self, read_to_string},
    io::stderr,
    path::{Path, PathBuf},
};

use anyhow::{anyhow, Result};
use clap::{CommandFactory, ErrorKind, Parser};
use fnscript::{syn::Script, tokenizer::tokenize};

#[derive(Parser)]
#[clap(allow_hyphen_values = true, disable_help_flag = true)]
struct Options {
    /// The script to execute
    script: PathBuf,
    /// The arguments for the script
    #[clap(allow_hyphen_values = true)]
    args: Vec<OsString>,
}

fn main() -> Result<()> {
    // let mut args = std::env::args().skip(1);
    // // TODO construct error messages with clap
    // let file = args.next().ok_or_else(|| anyhow!("Needs a script"))?;
    let Options { script: file, args } = Options::parse();
    match file.to_string_lossy().as_ref() {
        "-h" => {
            Options::into_app().write_help(&mut stderr())?;
            return Ok(());
        }
        "--help" => {
            Options::into_app().write_long_help(&mut stderr())?;
            return Ok(());
        }
        _ => {}
    }

    let script = &read_to_string(&file)?;
    let tokens = tokenize(script);
    let script: Script = tokens.to_parse().parse()?;

    let mut command = script.to_command(
        &file
            .file_name()
            .expect("Only files can be read")
            .to_string_lossy(),
    )?;
    let matches = match command.try_get_matches_from_mut(&args) {
        Ok(matches) => matches,
        Err(e) if matches!(e.kind(), ErrorKind::DisplayHelp | ErrorKind::DisplayVersion) => {
            script.print_help(command, &args)?;
            unreachable!()
        }
        Err(e) => e.exit(),
    };
    dbg!(matches);

    Ok(())
}
