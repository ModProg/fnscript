use std::{borrow::Cow, ffi::OsString, mem};

use anyhow::{anyhow, bail, Result};
use clap::Command;
use syn::{Function, Script};

pub mod syn;

pub mod tokenizer;

mod script;

impl Script {
    pub fn to_command(&self, file_name: &str) -> Result<Command> {
        let Self { fns, .. } = self;

        let mut command = Command::new(file_name);

        for Function { vis, name, .. } in fns {
            if vis.is_pub() {
                command = command.subcommand(Command::new(
                    // TODO implement actual errors like rust has
                    name.as_ref()
                        .ok_or_else(|| anyhow!("`pub` functions need a name: {:?}", vis.span))?
                        .to_string(),
                ));
            }
        }

        Ok(command.no_binary_name(true).bin_name(file_name))
    }

    pub fn print_help(&self, mut command: Command, args: Vec<OsString>) -> Result<()> {
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
        // TODO let mut bin_name = None;

        if let Some(title) = title {
            command = command.name(title);
        }
        if let Some(author) = author {
            command = command.author(author);
        }
        if let Some(version) = version {
            command = command.version(version);
        }
        if let Some(long_version) = long_version {
            command = command.long_version(long_version)
        }
        if let Some(long_about) = long_about {
            command = command.long_about(long_about)
        }
        if let Some(about) = about {
            command = command.about(about)
        }

        let sub_command_abouts: Vec<_> = fns
            .iter()
            .filter(|fun| fun.vis.is_pub())
            .map(|fun| fun.doc.as_ref().map(|d| slight_markdown(&d.comment)))
            .collect();
        for (about, subcommand) in sub_command_abouts.iter().zip(command.get_subcommands_mut()) {
            let s = mem::take(subcommand);
            *subcommand = s.about(about.as_deref());
        }

        command.get_matches_from(args);

        unreachable!("Should always print help");
    }
}

#[derive(Default, Debug)]
struct FrontMatter<'a> {
    title: Option<&'a str>,
    author: Option<&'a str>,
    version: Option<&'a str>,
    long_version: Option<&'a str>,
}

impl FrontMatter<'_> {
    fn from_doc(doc: &str) -> Result<(FrontMatter, &str)> {
        let mut fm = FrontMatter::default();

        let (fmp, rem) = get_paragraph(doc);

        for line in fmp.lines().filter_map(|line| {
            let line = line.trim();
            (!line.is_empty()).then(|| line)
        }) {
            if let Some(title) = line
                .strip_prefix('#')
                .or_else(|| line.strip_prefix("title:"))
            {
                if let Some(old) = fm.title {
                    bail!("Duplicate title entries in front matter (first paragraph of script doc comment `//!`): {old:?} and {title:?}\nEvery line starting with `#` or `title:` is identified as a title entry")
                }
                fm.title = Some(title.trim());
            } else if let Some(author) = line
                .strip_prefix("by ")
                .or_else(|| line.strip_prefix("author:"))
                .or_else(|| line.strip_prefix("authors:"))
            {
                if let Some(old) = fm.author {
                    bail!("Duplicate author entries in front matter (first paragraph of script doc comment `//!`): {old:?} and {author:?}\nEvery line starting with `by` or `author:` is identified as an author entry")
                }
                fm.author = Some(author);
            } else if let Some(version) = line
                .strip_prefix("version:")
                .or_else(|| line.strip_prefix('v'))
            {
                if let Some(old) = fm.version {
                    bail!("Duplicate version entries in front matter (first paragraph of script doc comment `//!`): {old:?} and {version:?}\nEvery line starting with `v` or `version:` is identified as a version entry")
                }
                if line.starts_with("version") {
                    fm.version = Some(version);
                } else {
                    let paren_open = version.find('(').unwrap_or_default();
                    if paren_open > 0 && version.ends_with(')') && fm.long_version.is_none() {
                        fm.version = Some(version[0..paren_open].trim_end());
                        fm.long_version = Some(version);
                    } else {
                        fm.version = Some(version)
                    }
                }
            } else if let Some(long_version) = line.strip_prefix("long version:") {
                // FIXME there is no exhaustive detection of duplicate long_version entries
                // as it is expected to overwrite a value set by `v`
                if let (Some(version), Some(long_version)) = (fm.version, fm.long_version) {
                    if long_version.starts_with(version) {
                        // This is most likely a wrongly detected "long version" using the `v` detection
                        fm.version = fm.long_version;
                    }
                } else if let Some(old) = fm.long_version {
                    bail!("Duplicate long version entries in front matter (first paragraph of script doc comment `//!`): {old:?} and {long_version:?}\nEvery line starting with `long version:` is identified as a long version entry")
                }
                fm.long_version = Some(long_version)
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
    doc.lines()
        .map(|l| {
            if l.starts_with(char::is_whitespace) {
                Cow::Owned(format!("\n{l}"))
            } else {
                Cow::Borrowed(l)
            }
        })
        .collect()
}
