use walkdir::WalkDir;
use gumdrop::Options;
use std::path::{PathBuf};

pub mod error;
pub mod parsing;
pub mod tst;

#[macro_use]
extern crate log;

#[macro_use]
extern crate ra_syntax;
use std::collections::HashMap;
use relative_path::{RelativePathBuf, RelativePath};
use crate::tst::{TreeFile, Item};
use crate::parsing::{ParseErrorWithPosAndFile, ParseError};
use std::io::Write;

/// Preprocessor
#[derive(gumdrop::Options)]
struct Args {
    help: bool,
    /// Override current directory
    path: Option<PathBuf>,
}

struct ParseResultAndContent {
    parsed: TreeFile,
    content: String,
}

fn main() {
    env_logger::init();

    let options: Args = Args::parse_args_default_or_exit();
    let mut current_dir = std::env::current_dir().expect("current dir");
    if let Some(path) = options.path {
        current_dir = path;
    }

    let mut all_files: HashMap<RelativePathBuf, String> = HashMap::new();
    for entry in WalkDir::new(&current_dir) {
        let entry = entry.unwrap();
        let file_name = entry.file_name().to_string_lossy();
        if file_name.ends_with(".gen.rs") {
            trace!("file {:?}", file_name);
            let rel_path = RelativePathBuf::from_path(
                entry.path().strip_prefix(&current_dir)
                .expect("strip current dir from file path"))
                .expect("create relative path");
            trace!("rel path {:?}", &rel_path);

            let content = std::fs::read_to_string(entry.path()).expect("read file");

            all_files.insert(rel_path, content);
        }
    }

    let all_parsed_files = match parse_all(
        all_files.iter().map(|(k, v)| (k.as_relative_path(), v.as_str()))
    ) {
        Ok(f) => f,
        Err(e) => {
            println!("in file {:?}", e.file);
            let data = all_files.get(&e.file).unwrap();
            println!("{}", error::display(data, e.range, &e.message));
            std::process::exit(42);
        }
    };

    for (rel_path, file) in all_parsed_files {
        let rel_output_file = rel_path.parent().expect("file parent")
            .join(format!("{}.rs", rel_path.file_name().unwrap().strip_suffix(".gen.rs").unwrap()));
        let output_file = rel_output_file.to_path(&current_dir);

        debug!("output {:?}", output_file);

        let mut output_file_contents = Vec::<u8>::new();
        writeln!(&mut output_file_contents, "// AUTO-GENERATED from {}", rel_path).unwrap();
        writeln!(&mut output_file_contents).unwrap();

        file.parsed.write(&file.content, &mut output_file_contents);

        let mut file = std::fs::OpenOptions::new()
            .create(true)
            .truncate(true)
            .append(false)
            .write(true)
            .open(output_file)
            .expect("failed to open file");

        file.write_all(&output_file_contents).expect("failed to write");
    }
}

fn parse_all<'q>(files: impl Iterator<Item=(&'q RelativePath, &'q str)>) -> Result<HashMap<RelativePathBuf, ParseResultAndContent>, ParseErrorWithPosAndFile> {
    let mut all_parsed_files: HashMap<RelativePathBuf, ParseResultAndContent> = HashMap::new();

    // parse

    for (rel_path, content) in files {
        all_parsed_files.insert(rel_path.to_owned(), ParseResultAndContent {
            parsed: TreeFile::parse(&rel_path, content)?,
            content: content.into(),
        });
    }

    // validate

    for (k, v) in all_parsed_files.iter() {
        for (_, tmp) in v.parsed.templates.iter() {
            tmp.check_parameters_used(&v.content)
                .map_err(|e| e.with_file(k))?;
        }

        for item in v.parsed.items.iter() {
            match item {
                Item::TemplateInvocation(inv) => {
                    let template = match v.parsed.templates.get(&inv.name) {
                        Some(t) => t,
                        None => {
                            return Err(ParseError::TypeError
                                .with("call references unknown template", inv.name_range)
                                .with_file(k))
                        }
                    };

                    use itertools::Itertools;

                    if template.args.len() != inv.args.len() {
                        if template.args.len() == 0 {
                            return Err(ParseError::TypeError
                                .with("does not require arguments", inv.arg_range)
                                .with_file(k))
                        } else if template.args.len() == 1 {
                            return Err(ParseError::TypeError
                                .with2(format!("requires 1 argument {:?}", template.args[0].1), inv.arg_range)
                                .with_file(k))
                        } else {
                            return Err(ParseError::TypeError
                                .with2(
                                    format!(
                                        "requires {} arguments: {}",
                                        template.args.len(),
                                        template.args.iter().map(|(_, name)| format!("{:?}", name)).join(", ")
                                    ),
                                    inv.arg_range)
                                .with_file(k))
                        }
                    }
                },
                Item::Content(_) => {},
            }
        }
    }

    Ok(all_parsed_files)
}

