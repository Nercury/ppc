use walkdir::WalkDir;
use gumdrop::Options;
use std::path::{PathBuf};

pub mod error;
pub mod parsing;
pub mod tst;
pub mod relt;
pub mod runtime;

#[macro_use]
extern crate log;

use std::collections::HashMap;
use relative_path::{RelativePathBuf};
use crate::tst::{GenerationPoint, Item, Expr, ModPath};
use crate::parsing::{ParseErrorWithPosAndFile, ParseError};
use std::io::Write;
use crate::runtime::Stack;
use ra_syntax::SyntaxKind;

/// Preprocessor
#[derive(gumdrop::Options)]
struct Args {
    help: bool,
    /// Override current directory
    path: Option<PathBuf>,
}

struct ParseResultAndContent {
    parsed: GenerationPoint,
    content: String,
    tmp_content: Option<String>,
}

#[derive(Copy, Clone)]
pub struct ParseConfig<'a> {
    pub gen_file_suffix: &'a str,
    pub tmp_file_suffix: &'a str,
}

fn main() {
    env_logger::init();

    let options: Args = Args::parse_args_default_or_exit();
    let mut current_dir = std::env::current_dir().expect("current dir");
    if let Some(path) = options.path {
        current_dir = path;
    }

    let config = ParseConfig {
        gen_file_suffix: ".gen.rs",
        tmp_file_suffix: ".gtp.rs",
    };

    let mut all_files: HashMap<RelativePathBuf, String> = HashMap::new();
    for entry in WalkDir::new(&current_dir) {
        let entry = entry.unwrap();
        let file_name = entry.file_name().to_string_lossy();
        if file_name.ends_with(config.gen_file_suffix) || file_name.ends_with(config.tmp_file_suffix) {
            trace!("file {:?}", file_name);
            let rel_path = RelativePathBuf::from_path(
                entry.path().strip_prefix(&current_dir)
                .expect("strip current dir from file path"))
                .expect("create relative path");
            trace!("rel path {:?}", &rel_path);

            if !rel_path.components().any(|c| c.as_str().contains("::")) {
                let content = std::fs::read_to_string(entry.path()).expect("read file");
                all_files.insert(rel_path, content);
            }
        }
    }

    let all_parsed_files = match parse_all(&all_files, config) {
        Ok(f) => f,
        Err(e) => {
            println!("in file {:?}", e.file);
            let data = all_files.get(&e.file).unwrap();
            println!("{}", error::display(data, e.range, &e.message));
            std::process::exit(42);
        }
    };

    for (_mod_path, file) in all_parsed_files {
        let generator_file_rel_path = file.parsed.generator.relative_path.as_relative_path();
        let rel_output_file = generator_file_rel_path.parent().expect("file parent")
            .join(format!("{}.rs", generator_file_rel_path.file_name().unwrap().strip_suffix(config.gen_file_suffix).unwrap()));
        let output_file = rel_output_file.to_path(&current_dir);

        debug!("output {:?}", output_file);

        let mut output_file_contents = Vec::<u8>::new();
        if let Some(ref template_file) = file.parsed.template {
            writeln!(&mut output_file_contents, "// AUTO-GENERATED from {} and {}",
                     generator_file_rel_path, template_file.relative_path).unwrap();
        } else {
            writeln!(&mut output_file_contents, "// AUTO-GENERATED from {}",
                     generator_file_rel_path).unwrap();
        }
        writeln!(&mut output_file_contents).unwrap();

        let mut stack = Stack::new();
        if let Err(e) = file.parsed.write(
            &file.content,
            file.tmp_content.as_ref().map(|s| &s[..]),
            &mut output_file_contents,
            &mut stack, None)
                .map_err(|e| e.with_file(&generator_file_rel_path)) {
            println!("in file {:?}", e.file);
            let data = all_files.get(&e.file).unwrap();
            println!("{}", error::display(data, e.range, &e.message));
            std::process::exit(42);
        }

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

fn parse_all(files: &HashMap<RelativePathBuf, String>, config: ParseConfig) -> Result<HashMap<ModPath, ParseResultAndContent>, ParseErrorWithPosAndFile> {
    let mut all_parsed_files: HashMap<ModPath, ParseResultAndContent> = HashMap::new();

    // parse

    for (rel_path, content) in files {
        if rel_path.file_name().unwrap().ends_with(config.gen_file_suffix) {
            let tmp_file_rel_path = rel_path.parent().unwrap().join(
                format!("{}{}", rel_path.file_name().unwrap().strip_suffix(config.gen_file_suffix).unwrap(), config.tmp_file_suffix)
            );
            let (tmp_rel_path, tmp_content) = if let Some(tmp_content) = files.get(&tmp_file_rel_path) {
                (Some(tmp_file_rel_path.as_relative_path()), Some(tmp_content.clone()))
            } else {
                (None, None)
            };
            all_parsed_files.insert(ModPath::from_generator_path(rel_path, config), ParseResultAndContent {
                parsed: GenerationPoint::parse(
                    &rel_path,
                    content,
                    tmp_rel_path,
                    tmp_content.as_ref().map(|s| &s[..])
                )?,
                content: content.into(),
                tmp_content,
            });
        }
    }

    // validate

    for (_mod_path, v) in all_parsed_files.iter() {
        for (_, tmp) in v.parsed.generator.templates.iter() {
            tmp.check_parameters_used(&v.content)
                .map_err(|e| e.with_file(&v.parsed.generator.relative_path))?;
        }
        validate_items(v.content.as_ref(), v.tmp_content.as_ref().map(|s| &s[..]), &v.parsed.generator.items, &v.parsed)?;

        if let (&Some(ref template), &Some(ref tmp_content)) = (&v.parsed.template, &v.tmp_content) {
            for (_, tmp) in template.templates.iter() {
                tmp.check_parameters_used(tmp_content)
                    .map_err(|e| e.with_file(&template.relative_path))?;
            }
        }
    }

    Ok(all_parsed_files)
}

fn validate_items(data: &str, tmp_data: Option<&str>, generator_items: &[Item], generation_point: &GenerationPoint) -> Result<(), ParseErrorWithPosAndFile> {
    for item in generator_items.iter() {
        match item {
            Item::TemplateInvocation(inv) => {
                let (template, _) = match generation_point.find_template(&inv.name, data, tmp_data) {
                    Some(t) => t,
                    None => {
                        return Err(
                            ParseError::TypeError
                                .with("call references unknown template", inv.name_range)
                                .with_file(&generation_point.generator.relative_path)
                        )
                    }
                };

                use itertools::Itertools;

                if template.args.len() != inv.args.len() {
                    if template.args.len() == 0 {
                        return Err(ParseError::TypeError
                            .with("does not require arguments", inv.arg_range)
                            .with_file(&generation_point.generator.relative_path))
                    } else if template.args.len() == 1 {
                        return Err(ParseError::TypeError
                            .with2(format!("requires 1 argument {:?}", template.args[0].1), inv.arg_range)
                            .with_file(&generation_point.generator.relative_path))
                    } else {
                        return Err(ParseError::TypeError
                            .with2(
                                format!(
                                    "requires {} arguments: {}",
                                    template.args.len(),
                                    template.args.iter().map(|(_, name)| format!("{:?}", name)).join(", ")
                                ),
                                inv.arg_range)
                            .with_file(&generation_point.generator.relative_path))
                    }
                }

                for arg in &inv.args {
                    if arg.len() > 1 && arg[0].kind() == SyntaxKind::BANG {
                        if arg[1].kind() != SyntaxKind::IDENT {
                            return Err(ParseError::TypeError
                                .with("expected IDENT for a named argument", arg[1].text_range())
                                .with_file(&generation_point.generator.relative_path))
                        }

                        if arg.len() > 2 {
                            return Err(ParseError::TypeError
                                .with("unexpected token for a named argument", arg[2].text_range())
                                .with_file(&generation_point.generator.relative_path))
                        }
                    }
                }
            },
            Item::Content(_) => {},
            Item::Expr(e) => match e {
                Expr::ForRange { items, .. } => {
                    validate_items(data, tmp_data, &items, generation_point)?;
                },
            },
            Item::Block { items: block_items, .. } => {
                validate_items(data, tmp_data, &block_items, generation_point)?;
            }
        }
    }

    Ok(())
}

