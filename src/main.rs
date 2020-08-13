use walkdir::WalkDir;
use gumdrop::Options;
use std::path::{PathBuf};

pub mod error;
pub mod parsing;

#[macro_use]
extern crate ra_syntax;
use ra_syntax::{ast, AstNode, SyntaxKind, TextRange};
use std::collections::HashMap;
use parsing::Expected;
use crate::parsing::{ParseErrorWithPos, ParseError, expect_any};
use std::io::Write;
use relative_path::{RelativePathBuf};

/// Preprocessor
#[derive(gumdrop::Options)]
struct Args {
    help: bool,
    /// Override current directory
    path: Option<PathBuf>,
}

fn main() {
    let options: Args = Args::parse_args_default_or_exit();
    let mut current_dir = std::env::current_dir().expect("current dir");
    if let Some(path) = options.path {
        current_dir = path;
    }

    for entry in WalkDir::new(&current_dir) {
        let entry = entry.unwrap();
        let file_name = entry.file_name().to_string_lossy();
        if file_name.ends_with(".gen.rs") {
            let rel_path = RelativePathBuf::from_path(entry.path().strip_prefix(&current_dir)
                .expect("strip current dir from file path"))
                .expect("create relative path");

            let data = std::fs::read_to_string(entry.path()).expect("read file");
            let output_file = entry.path().parent().expect("file parent")
                .join(format!("{}.rs", file_name.strip_suffix(".gen.rs").unwrap()));

            let mut output_file_contents = String::new();

            let parsed = ra_syntax::SourceFile::parse(&data);

            if parsed.errors().len() > 0 {
                println!("in file {:?}", entry.path());
                for e in parsed.errors() {
                    println!("{}", error::display(&data, e.range(), &e.to_string()));
                    std::process::exit(42);
                }
            } else {
                let tree = parsed.tree();

                let mut templates = HashMap::new();
                for node in tree.syntax().descendants() {
                    match_ast! {
                        match node {
                            ast::Item(it) => {
                                match it {
                                    ast::Item::MacroCall(macro_call)
                                        => match make_template_from_macro(&macro_call) {
                                        Ok(template) => {
                                            templates.insert(template.name.clone(), template);
                                        },
                                        Err(Some(e)) => {
                                            println!("in file {:?}", entry.path());
                                            println!("{}", error::display(&data, e.range, &e.message));
                                            std::process::exit(43);
                                        },
                                        Err(None) => (),
                                    },
                                    _ => (),
                                };
                            },
                            _ => (),
                        }
                    }
                }

                let mut something_was_printed = false;
                let mut last_item_end = None;

                for node in tree.syntax().descendants() {
                    match_ast! {
                        match node {
                            ast::Item(it) => {
                                let (should_output, alternative_output) = match it {
                                    ast::Item::MacroCall(ref macro_call) =>
                                        if is_template(macro_call) {
                                            (false, None)
                                        } else {
                                            if let Some(macro_call_name) = get_macro_call_name(&macro_call) {
                                                if let Some(template) = templates.get(&macro_call_name) {
                                                    match parse_template_invocation(template, macro_call) {
                                                        Ok(inv) => (true, Some(inv.produce(&data, template))),
                                                        Err(e) => {
                                                            println!("in file {:?}", entry.path());
                                                            println!("{}", error::display(&data, e.range, &e.message));
                                                            std::process::exit(44);
                                                        }
                                                    }
                                                } else {
                                                    (true, None)
                                                }
                                            } else {
                                                (false, None)
                                            }
                                        }
                                    _ => (true, None),
                                };

                                if should_output {
                                    if let Some(start) = it.syntax().first_token().map(|token| u32::from(token.text_range().start())) {
                                        // whitespace handling, leave the exact space tokens from the previous item
                                        if let (true, Some(end)) = (something_was_printed, last_item_end) {
                                            let snippet = String::from_utf8_lossy(&data.as_bytes()[end as usize..start as usize]);
                                            output_file_contents.push_str(&snippet);
                                        }

                                        if let Some(alt) = alternative_output {
                                            output_file_contents.push_str(&alt);
                                        } else {
                                            output_file_contents.push_str(&it.to_string());
                                        }

                                        something_was_printed = true;
                                    }
                                }

                                last_item_end = it.syntax().last_token().map(|token| u32::from(token.text_range().end()))
                            },
                            _ => (),
                        }
                    }
                }

                let mut file = std::fs::OpenOptions::new()
                    .create(true)
                    .truncate(true)
                    .append(false)
                    .write(true)
                    .open(output_file)
                    .expect("failed to open file");

                writeln!(file, "// AUTO-GENERATED from {}", rel_path).expect("write comment");
                writeln!(file).expect("writeln");
                file.write_all(output_file_contents.as_bytes()).expect("failed to write");
            }
        }
    }
}

struct TemplateInvocation {
    pub args: Vec<TextRange>,
}

impl TemplateInvocation {
    pub fn produce(&self, data: &str, template: &Template) -> String {
        let input_bytes = &data.as_bytes()[template.template_start..template.template_end];
        let (bytes, start_newline) = if input_bytes.starts_with(b"\n") {
            (&input_bytes[1..], &input_bytes[..1])
        } else if input_bytes.starts_with(b"\r\n") {
            (&input_bytes[2..], &input_bytes[..2])
        } else {
            (input_bytes, &b""[..])
        };

        // you get your indent removed it it is consistent with the first line
        let mut whitespace_ident = Vec::new();
        for b in bytes {
            if !(b.is_ascii_whitespace() && *b != b'\r' && *b != b'\n') {
                break;
            }
            whitespace_ident.push(*b);
        }

        let mut adjusted_template = String::from_utf8_lossy(start_newline).into_owned();
        for line in LinesIter::new(bytes) {
            if line.starts_with(&whitespace_ident[..]) {
                adjusted_template.push_str(std::str::from_utf8(&line[whitespace_ident.len()..]).expect("line from utf bytes"));
            } else {
                adjusted_template.push_str(std::str::from_utf8(&line).expect("line from utf bytes 2"));
            }
        }

        let mut replacements = HashMap::new();
        for (arg_name, arg_value) in template.args.iter().zip(self.args.iter()) {
            let value_text = &data.as_bytes()[u32::from(arg_value.start()) as usize..u32::from(arg_value.end()) as usize];
            let replace_text = format!("<{}>", arg_name);
            replacements.insert(replace_text, String::from_utf8_lossy(value_text).into_owned());
        }

        let adjusted_template_bytes = adjusted_template.as_bytes();
        let mut final_template = String::new();

        let mut block_start = 0;
        let mut i = 0;
        'replace: loop {
            let bytes_at_i = &adjusted_template_bytes[i..];

            for (search, replace) in replacements.iter() {
                let search_as_bytes = search.as_bytes();
                if bytes_at_i.starts_with(search.as_bytes()) {
                    final_template.push_str(std::str::from_utf8(&adjusted_template_bytes[block_start..i]).expect("no utf8 problems"));
                    final_template.push_str(replace);
                    block_start = i + search_as_bytes.len();
                    i = block_start;
                    continue 'replace;
                }
            }

            i = i + 1;
            if i >= adjusted_template_bytes.len() {
                break;
            }
        }
        if block_start < adjusted_template_bytes.len() {
            final_template.push_str(std::str::from_utf8(&adjusted_template_bytes[block_start..]).expect("no utf8 problems"));
        }

        final_template
    }
}

struct LinesIter<'a> {
    data: &'a [u8],
    line_start: usize,
}

impl<'a> LinesIter<'a> {
    pub fn new(data: &'a [u8]) -> LinesIter<'a> {
        LinesIter {
            data,
            line_start: 0,
        }
    }
}

impl<'a> Iterator for LinesIter<'a> {
    type Item = &'a [u8];

    fn next(&mut self) -> Option<Self::Item> {
        if self.line_start >= self.data.len() {
            None
        } else {
            for (offset, c) in self.data[self.line_start..].iter().enumerate() {
                if *c == b'\n' {
                    // i am aware that this does not handle \r
                    // actually this is a feature, it preserves \r
                    let previous_start = self.line_start;
                    self.line_start = self.line_start + offset + 1;
                    return Some(&self.data[previous_start..self.line_start]);
                }
            }
            let previous_start = self.line_start;
            self.line_start = self.data.len();
            return Some(&self.data[previous_start..self.line_start]);
        }
    }
}

struct Template {
    pub name: String,
    pub args: Vec<String>,
    pub template_start: usize,
    pub template_end: usize,
}

fn is_template(macro_call: &ast::MacroCall) -> bool {
    get_template_macro_call(&macro_call).is_some()
}

fn get_template_macro_call(macro_call: &ast::MacroCall) -> Option<&ast::MacroCall> {
    if let Some(_rules) = macro_call.is_macro_rules() {
        return Some(macro_call);
    }

    None
}

fn get_macro_call_name(macro_call: &ast::MacroCall) -> Option<String> {
    let mut token = macro_call.syntax().first_token()?;
    while token.kind() != SyntaxKind::IDENT {
        token = parsing::expect_next(token).ok()?;
    }
    Some(token.text().to_string())
}

fn parse_template_invocation(template: &Template, macro_call: &ast::MacroCall) -> Result<TemplateInvocation, ParseErrorWithPos> {
    let mut token = macro_call.syntax().first_token().unwrap();
    while token.kind() != SyntaxKind::IDENT {
        token = parsing::expect_next(token)?;
    }

    token = parsing::consume_expected_list_forward(token, &[
        Expected::Text(&template.name),
        Expected::Kind(SyntaxKind::BANG),
        Expected::Kind(SyntaxKind::L_PAREN),
    ])?;

    let mut args = Vec::new();

    'main: for (i, arg) in template.args.iter().enumerate() {
        let start_token = token.clone();
        let mut last_token = token.clone();
        let mut arg_start = None;
        let mut arg_end = None;
        let is_last = i + 1 == template.args.len();

        'consume: loop {
            if token.kind() == SyntaxKind::COMMA {
                if is_last {
                    if let Some(next_token_peek) = token.next_token() {
                        // let's be good and allow for trailing comma if the next item is closed parenthesis
                        let (next_ident_peek_token, found_ix) = expect_any(next_token_peek, &[
                            Expected::Kind(SyntaxKind::R_PAREN),
                            Expected::Kind(SyntaxKind::IDENT),
                            Expected::Optional(Box::new(Expected::Kind(SyntaxKind::WHITESPACE)))
                        ])?;
                        if found_ix == 1 {
                            return Err(ParseError::ExpectedOtherToken.with(
                                "unexpected item beyond the last argument",
                                next_ident_peek_token.text_range()
                            ));
                        }
                    }
                }

                if let (Some(start), Some(end)) = (arg_start, arg_end) {
                    args.push(TextRange::new(start, end));
                    token = parsing::expect_next(token)?;
                    break 'consume;
                } else {
                    return Err(ParseError::ExpectedOtherToken.with2(
                        format!("expected tokens for argument {}", arg),
                        TextRange::new(start_token.text_range().start(), last_token.text_range().end())
                    ));
                }
            } else if token.kind() == SyntaxKind::R_PAREN {
                if !is_last {
                    return Err(ParseError::ExpectedOtherToken.with2(
                        format!("missing argument {}", template.args[i+1]),
                        token.text_range()
                    ));
                }

                if let (Some(start), Some(end)) = (arg_start, arg_end) {
                    args.push(TextRange::new(start, end));
                    token = parsing::expect_next(token)?;
                    continue 'main;
                } else {
                    return Err(ParseError::ExpectedOtherToken.with2(
                        format!("expected tokens for argument {}", arg),
                        TextRange::new(start_token.text_range().start(), last_token.text_range().end())
                    ));
                }
            } else if token.kind() == SyntaxKind::WHITESPACE {
            } else {
                if arg_start == None {
                    arg_start = Some(token.text_range().start());
                }
                arg_end = Some(token.text_range().end())
            }
            last_token = token.clone();
            token = parsing::expect_next(token)?;
        }
    }

    Ok(TemplateInvocation {
        args,
    })
}

fn make_template_from_macro(macro_call: &ast::MacroCall) -> Result<Template, Option<ParseErrorWithPos>> {
    if let Some(rules) = macro_call.is_macro_rules() {
        let name = rules.to_string();

        let mut start_token = macro_call.syntax().first_token().ok_or(None)?;
        let mut end_token = macro_call.syntax().last_token().ok_or(None)?;

        start_token = parsing::consume_expected_list_forward(start_token, &[
            Expected::Text("macro_rules"),
            Expected::Kind(SyntaxKind::BANG),
            Expected::Kind(SyntaxKind::WHITESPACE),
            Expected::KindAndText(SyntaxKind::IDENT, &name),
            Expected::Optional(Box::new(Expected::Kind(SyntaxKind::WHITESPACE))),
            Expected::Kind(SyntaxKind::L_CURLY),
            Expected::Optional(Box::new(Expected::Kind(SyntaxKind::WHITESPACE))),
            Expected::Kind(SyntaxKind::L_PAREN),
        ])?;

        let mut args = Vec::new();

        let (next_token, which) = parsing::expect_any(start_token, &[
            Expected::Kind(SyntaxKind::IDENT),
            Expected::Kind(SyntaxKind::R_PAREN),
            Expected::Optional(Box::new(Expected::Kind(SyntaxKind::WHITESPACE))),
        ])?;
        start_token = next_token;
        if which == 0 {
            args.push(start_token.text().to_string());
            start_token = parsing::expect_next(start_token)?;

            loop {
                let (next_token, which) = parsing::expect_any(start_token, &[
                    Expected::Kind(SyntaxKind::COMMA),
                    Expected::Kind(SyntaxKind::R_PAREN),
                    Expected::Optional(Box::new(Expected::Kind(SyntaxKind::WHITESPACE))),
                ])?;
                start_token = next_token;
                start_token = parsing::expect_next(start_token)?;
                if which == 1 {
                    break;
                } else {
                    let (next_token, which) = parsing::expect_any(start_token, &[
                        Expected::Kind(SyntaxKind::IDENT),
                        Expected::Optional(Box::new(Expected::Kind(SyntaxKind::WHITESPACE))),
                    ])?;
                    start_token = next_token;
                    if which == 0 {
                        args.push(start_token.text().to_string());
                    }
                    start_token = parsing::expect_next(start_token)?;
                }
            }
        }

        start_token = parsing::consume_expected_list_forward(start_token, &[
            Expected::Optional(Box::new(Expected::Kind(SyntaxKind::WHITESPACE))),
            Expected::Kind(SyntaxKind::EQ),
            Expected::Kind(SyntaxKind::R_ANGLE),
            Expected::Optional(Box::new(Expected::Kind(SyntaxKind::WHITESPACE))),
            Expected::Kind(SyntaxKind::L_CURLY),
        ])?;

        end_token = parsing::consume_expected_list_backward(end_token, &[
            Expected::Kind(SyntaxKind::R_CURLY),
            Expected::Optional(Box::new(Expected::Kind(SyntaxKind::WHITESPACE))),
            Expected::Kind(SyntaxKind::R_CURLY),
            Expected::Optional(Box::new(Expected::Kind(SyntaxKind::WHITESPACE))),
        ])?;

        let template_start = u32::from(start_token.text_range().start());
        let template_end = u32::from(end_token.text_range().end());

        return Ok(Template {
            name,
            args,
            template_start: template_start as usize,
            template_end: template_end as usize,
        });
    }
    Err(None)
}

