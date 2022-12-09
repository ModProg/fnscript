use std::{convert::Infallible, ffi::OsString, mem};

use crate::syn::{Argument, Function, Script};
use anyhow::{anyhow, bail, Result};
use clap::{
    builder::{
        Resettable::{self, *},
        Str, StyledStr,
    },
    Arg, Command,
};

impl Script {
    pub fn to_command(&self, file_name: String) -> Result<Command> {
        let Self { fns, .. } = self;

        let mut command = Command::new(&file_name);

        for Function {
            vis, name, args, ..
        } in fns
        {
            if vis.is_pub() {
                command = command.subcommand(
                    Command::new(
                        // TODO implement actual errors like rust has
                        name.as_ref()
                            .ok_or_else(|| anyhow!("`pub` functions need a name: {:?}", vis.span))?
                            .to_string(),
                    )
                    .args(args),
                );
            }
        }

        // TODO support scripts without subcommands
        Ok(command
            .no_binary_name(true)
            .bin_name(file_name)
            .subcommand_required(true))
    }

    pub fn print_help(&self, command: Command, args: &[OsString]) -> Result<Infallible> {
        let Self { doc, fns } = self;

        let doc = doc.as_ref().map(|d| d.comment.as_str()).unwrap_or_default();
        let (
            FrontMatter {
                title,
                author,
                version,
                long_version,
            },
            doc,
        ) = FrontMatter::from_doc(doc)?;
        let (about, long_about) = get_paragraph(doc);
        let long_about = if long_about.is_empty() {
            None
        } else {
            Some(doc)
        };
        let about = if about.is_empty() { None } else { Some(about) };

        let long_about = long_about.map(slight_markdown);
        let about = about.map(slight_markdown);

        // TODO remove when https://github.com/clap-rs/clap/issues/4183 is fixed
        let mut command = self
            .to_command(command.get_bin_name().unwrap().to_string())
            .unwrap();

        command = command.display_name(title);
        command = command.author(author);
        command = command.version(version);
        command = command.long_version(long_version);
        command = command.long_about(string_to_styled(long_about));
        command = command.about(string_to_styled(about));
        for (fun, subcommand) in fns
            .iter()
            .filter(|fun| fun.vis.is_pub())
            .zip(command.get_subcommands_mut())
        {
            let s = mem::take(subcommand);
            *subcommand = s.about(string_to_styled(
                fun.doc.as_ref().map(|d| slight_markdown(&d.comment)),
            ));
        }

        command.build();
        command.get_matches_from(args);

        unreachable!("Should always print help");
    }
}

fn string_to_styled(value: Option<String>) -> Resettable<StyledStr> {
    Resettable::from(value.map(StyledStr::from))
}

#[derive(Debug)]
struct FrontMatter {
    title: Resettable<String>,
    author: Resettable<Str>,
    version: Resettable<Str>,
    long_version: Resettable<Str>,
}
impl Default for FrontMatter {
    fn default() -> Self {
        Self {
            title: Reset,
            author: Reset,
            version: Reset,
            long_version: Reset,
        }
    }
}

impl FrontMatter {
    fn from_doc(doc: &str) -> Result<(FrontMatter, &str)> {
        let mut fm = FrontMatter::default();

        let (fmp, rem) = get_paragraph(doc);

        for line in fmp.lines().filter_map(|line| {
            let line = line.trim();
            (!line.is_empty()).then_some(line)
        }) {
            if let Some(title) = line
                .strip_prefix('#')
                .or_else(|| line.strip_prefix("title:"))
            {
                if let Value(old) = fm.title {
                    bail!("Duplicate title entries in front matter (first paragraph of script doc comment `//!`): {old:?} and {title:?}\nEvery line starting with `#` or `title:` is identified as a title entry")
                }
                fm.title = Value(title.trim().to_string());
            } else if let Some(author) = line
                .strip_prefix("by ")
                .or_else(|| line.strip_prefix("author:"))
                .or_else(|| line.strip_prefix("authors:"))
            {
                if let Value(old) = fm.author {
                    bail!("Duplicate author entries in front matter (first paragraph of script doc comment `//!`): {old:?} and {author:?}\nEvery line starting with `by` or `author:` is identified as an author entry")
                }
                fm.author = Value(author.to_string().into());
            } else if let Some(version) = line
                .strip_prefix("version:")
                .or_else(|| line.strip_prefix('v'))
            {
                if let Value(old) = fm.version {
                    bail!("Duplicate version entries in front matter (first paragraph of script doc comment `//!`): {old:?} and {version:?}\nEvery line starting with `v` or `version:` is identified as a version entry")
                }
                if line.starts_with("version") {
                    fm.version = Value(version.to_string().into());
                } else {
                    let paren_open = version.find('(').unwrap_or_default();
                    if paren_open > 0 && version.ends_with(')') && matches!(fm.long_version, Reset)
                    {
                        fm.version = Value(version[0..paren_open].trim_end().to_string().into());
                        fm.long_version = Value(version.to_string().into());
                    } else {
                        fm.version = Value(version.to_string().into())
                    }
                }
            } else if let Some(long_version) = line.strip_prefix("long version:") {
                // FIXME there is no exhaustive detection of duplicate long_version entries
                // as it is expected to overwrite a value set by `v`
                if let (Value(version), Value(long_version)) = (&fm.version, &fm.long_version) {
                    if long_version.starts_with(version.as_str()) {
                        // This is most likely a wrongly detected "long version" using the `v` detection
                        fm.version = fm.long_version;
                    }
                } else if let Value(old) = fm.long_version {
                    bail!("Duplicate long version entries in front matter (first paragraph of script doc comment `//!`): {old:?} and {long_version:?}\nEvery line starting with `long version:` is identified as a long version entry")
                }
                fm.long_version = Value(long_version.to_string().into())
            } else {
                return Ok((FrontMatter::default(), doc));
            }
        }

        Ok((fm, rem))
    }
}

fn get_paragraph(doc: &str) -> (&str, &str) {
    doc.trim()
        .split_once("\n\n")
        .map(|(a, b)| (a.trim(), b.trim()))
        .unwrap_or_else(|| (doc.trim(), ""))
}

fn slight_markdown(doc: &str) -> String {
    let mut ret = String::with_capacity(doc.len());
    for line in doc.lines() {
        if line.starts_with(char::is_whitespace) || line.is_empty() {
            if !ret.ends_with('\n') {
                ret.push('\n')
            }
            ret.push_str(line);
        } else {
            if !ret.ends_with(char::is_whitespace) {
                ret.push(' ')
            }
            ret.push_str(line)
        }
    }
    ret
}

impl From<&Argument> for Arg {
    fn from(Argument { name, .. }: &Argument) -> Self {
        Arg::new(name.to_string())
            .value_name(name.to_string().to_uppercase())
            .required(true)
        // TODO implement dynamic completion whenever that is relevant
    }
}
