//! Template syntax tree

use relative_path::{RelativePathBuf, RelativePath};
use ra_syntax::{TextRange, ast, AstNode, SyntaxKind};
use std::collections::HashMap;
use crate::parsing::{ParseErrorWithPos, Expected, ParseError, ParseErrorWithPosAndFile};
use crate::{parsing};
use std::io::Write;

pub struct TreeFile {
    pub relative_path: RelativePathBuf,
    pub items: Vec<Item>,
    pub templates: HashMap<String, Template>,
}

pub enum Item {
    TemplateInvocation(TemplateInvocation),
    Content(String),
}

impl TreeFile {
    pub fn parse(relative_path: &RelativePath, data: &str) -> Result<TreeFile, ParseErrorWithPosAndFile> {

        let parsed = ra_syntax::SourceFile::parse(&data);

        trace!("error count {:?}", parsed.errors().len());

        for e in parsed.errors() {
            return Err(
                ParseError::SyntaxError
                    .with2(e.to_string(), e.range())
                    .with_file(relative_path)
            );
        }

        let tree = parsed.tree();

        let mut templates = HashMap::new();
        let mut items = Vec::new();

        let mut something_was_printed = false;
        let mut last_item_end = None;

        for node in tree.syntax().descendants() {
            match_ast! {
                match node {
                    ast::Item(it) => {
                        let syntax = it.syntax();
                        trace!("node item {:?}", syntax);
                        match it {
                            ast::Item::MacroCall(ref macro_call) => {
                                if let Some(_) = macro_call.is_macro_rules() {
                                    trace!("parse as template {:?}", macro_call);
                                    match Template::parse(&macro_call) {
                                        Ok(template) => {
                                            templates.insert(template.name.clone(), template);
                                            something_was_printed = true;
                                        },
                                        Err(e) => {
                                            return Err(
                                                ParseError::SyntaxError
                                                    .with2(e.message, e.range)
                                                    .with_file(relative_path)
                                            );
                                        },
                                    }
                                } else {
                                    trace!("parse as template use {:?}", macro_call);
                                    match TemplateInvocation::parse(&macro_call) {
                                        Ok(inv) => {
                                            items.push(Item::TemplateInvocation(inv));
                                            something_was_printed = true;
                                        },
                                        Err(e) => {
                                            return Err(
                                                ParseError::SyntaxError
                                                    .with2(e.message, e.range)
                                                    .with_file(relative_path)
                                            );
                                        }
                                    }
                                }
                            }
                            _ => {
                                if let Some(start) = syntax.first_token().map(|token| u32::from(token.text_range().start())) {
                                    // whitespace handling, leave the exact space tokens from the previous item
                                    if let (true, Some(end)) = (something_was_printed, last_item_end) {
                                        let snippet = String::from_utf8_lossy(&data.as_bytes()[end as usize..start as usize]).into_owned();
                                        items.push(Item::Content(snippet));
                                    }

                                    items.push(Item::Content(it.to_string()));
                                    something_was_printed = true;
                                }
                            }
                        }
                        last_item_end = it.syntax().last_token().map(|token| u32::from(token.text_range().end()));
                    },
                    _ => (),
                }
            }
        }

        Ok(TreeFile {
            relative_path: relative_path.to_owned(),
            templates,
            items,
        })
    }

    pub fn write(&self, data: &str, output: &mut impl Write) {
        for item in self.items.iter() {
            match item {
                Item::TemplateInvocation(inv) => {
                    match self.templates.get(&inv.name) {
                        None => warn!("template not located by name {}", inv.name),
                        Some(template) => write!(output, "{}", inv.produce(data, template)).unwrap(),
                    }
                },
                Item::Content(c) => write!(output, "{}", c).unwrap(),
            }
        }
    }
}

pub struct Template {
    pub name: String,
    pub args: Vec<(TextRange, String)>,
    pub template_start: usize,
    pub template_end: usize,
}

impl Template {
    pub fn parse(macro_call: &ast::MacroCall) -> Result<Template, ParseErrorWithPos> {
        let rules = macro_call.is_macro_rules().unwrap();
        let name = rules.to_string();
        trace!("name {:?}", name);

        let mut start_token = macro_call.syntax().first_token().unwrap();
        let mut end_token = macro_call.syntax().last_token().unwrap();

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
            args.push((start_token.text_range(), start_token.text().to_string()));
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
                        args.push((start_token.text_range(), start_token.text().to_string()));
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

    pub fn check_parameters_used(&self, data: &str) -> Result<(), ParseErrorWithPos> {
        let template = std::str::from_utf8(self.get_bytes(data)).expect("no utf8 issues");
        for (range, arg) in self.lookup_args() {
            if !template.contains(&arg) {
                return Err(
                    ParseError::TypeError
                        .with2(format!("parameter is not used in {:?}", self.name), range)
                );
            }
        }
        Ok(())
    }

    pub fn get_bytes<'a, 'q>(&'q self, data: &'a str) -> &'a [u8] {
        &data.as_bytes()[self.template_start..self.template_end]
    }

    pub fn lookup_args<'q>(&'q self) -> impl Iterator<Item=(TextRange, String)> + 'q {
        self.args.iter()
            .map(move |(range, arg_name)| (range.clone(), format!("<{}>", arg_name)))
    }
}

pub struct TemplateInvocation {
    pub name: String,
    pub args: Vec<TextRange>,
    pub name_range: TextRange,
    pub arg_range: TextRange,
}

impl TemplateInvocation {
    pub fn parse(macro_call: &ast::MacroCall) -> Result<TemplateInvocation, ParseErrorWithPos> {
        let mut token = macro_call.syntax().first_token().unwrap();
        while token.kind() != SyntaxKind::IDENT {
            token = parsing::expect_next(token)?;
        }

        let name = token.text().to_string();
        let name_range = token.text_range();
        trace!("name {:?}", name);

        token = parsing::expect_next(token)?;

        token = parsing::consume_expected_list_forward(token, &[
            Expected::Kind(SyntaxKind::BANG),
            Expected::Kind(SyntaxKind::L_PAREN),
        ])?;

        let args_start = token.text_range().start();
        let args_end;

        let mut args = Vec::new();

        'main: loop {
            let start_token = token.clone();
            let mut last_token = token.clone();
            let mut arg_start = None;
            let mut arg_end = None;

            'consume: loop {
                if token.kind() == SyntaxKind::COMMA {
                    if let (Some(start), Some(end)) = (arg_start, arg_end) {
                        args.push(TextRange::new(start, end));
                        token = parsing::expect_next(token)?;
                        break 'consume;
                    } else {
                        return Err(ParseError::ExpectedOtherToken.with(
                            "expected tokens for argument",
                            TextRange::new(start_token.text_range().start(), last_token.text_range().end())
                        ));
                    }
                } else if token.kind() == SyntaxKind::R_PAREN {
                    if let (Some(start), Some(end)) = (arg_start, arg_end) {
                        args.push(TextRange::new(start, end));
                    }
                    args_end = token.prev_token().unwrap().text_range().end();
                    token = parsing::expect_next(token)?;
                    break 'main;
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

        let _ = token;

        Ok(TemplateInvocation {
            name,
            args,
            name_range,
            arg_range: TextRange::new(args_start, args_end),
        })
    }

    pub fn produce(&self, data: &str, template: &Template) -> String {
        let input_bytes = template.get_bytes(data);
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
        for (replace_text, arg_value) in template.lookup_args().zip(self.args.iter()) {
            let value_text = &data.as_bytes()[u32::from(arg_value.start()) as usize..u32::from(arg_value.end()) as usize];
            replacements.insert(replace_text, String::from_utf8_lossy(value_text).into_owned());
        }

        let adjusted_template_bytes = adjusted_template.as_bytes();
        let mut final_template = String::new();

        let mut block_start = 0;
        let mut i = 0;
        'replace: loop {
            let bytes_at_i = &adjusted_template_bytes[i..];

            for ((_, search), replace) in replacements.iter() {
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