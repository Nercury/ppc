//! Template syntax tree

use relative_path::{RelativePathBuf, RelativePath};
use syntax::{TextRange, ast, AstNode, SyntaxKind, SyntaxToken};
use std::collections::HashMap;
use crate::parsing::{ParseErrorWithPos, Expected, ParseError, ParseErrorWithPosAndFile};
use crate::{parsing, ParseConfig, relt};
use std::io::Write;
use crate::runtime::{Stack, Varying};
use itertools::Itertools;
use crate::relt::{ReltNode};

pub enum Range {
    Numeric { from: i64, to: i64 },
}

pub enum Expr {
    ForRange { ident: String, range: Range, items: Vec<Item> },
}

impl Expr {
    pub fn parse_from_macro(data: &str, remaining: &mut &[ReltNode], token: SyntaxToken) -> Result<Expr, ParseErrorWithPos> {
        Ok(match token.text().as_str() {
            "for_range" => Self::parse_for_range(data, remaining, token)?,
            "end" => return Err(ParseError::ExpectedOtherToken.with("no matching start", token.text_range())),
            _=> unreachable!("did is_expr_start work"),
        })
    }

    fn parse_for_range(data: &str, remaining: &mut &[ReltNode], mut token: SyntaxToken) -> Result<Expr, ParseErrorWithPos> {
        let expr_start_token = token;
        token = parsing::expect_next(expr_start_token.clone())?;
        token = parsing::consume_expected_list_forward(token, &[
            Expected::Kind(SyntaxKind::BANG),
            Expected::Optional(Box::new(Expected::Kind(SyntaxKind::WHITESPACE))),
            Expected::Kind(SyntaxKind::L_PAREN),
            Expected::Optional(Box::new(Expected::Kind(SyntaxKind::WHITESPACE))),
        ])?;

        if token.kind() != SyntaxKind::IDENT {
            return Err(ParseError::ExpectedOtherToken.with("expected IDENT", token.text_range()));
        }

        let ident_text = token.text().to_string();
        token = parsing::expect_next(token)?;
        token = parsing::consume_expected_list_forward(token, &[
            Expected::Optional(Box::new(Expected::Kind(SyntaxKind::WHITESPACE))),
            Expected::Kind(SyntaxKind::COMMA),
            Expected::Optional(Box::new(Expected::Kind(SyntaxKind::WHITESPACE))),
        ])?;

        if token.kind() != SyntaxKind::INT_NUMBER {
            return Err(ParseError::ExpectedOtherToken.with("expected INT_NUMBER", token.text_range()));
        }

        let range_start_token = token.clone();
        token = parsing::expect_next(token)?;
        token = parsing::consume_expected_list_forward(token, &[
            Expected::Optional(Box::new(Expected::Kind(SyntaxKind::WHITESPACE))),
            Expected::Kind(SyntaxKind::DOT),
            Expected::Kind(SyntaxKind::DOT),
            Expected::Optional(Box::new(Expected::Kind(SyntaxKind::WHITESPACE))),
        ])?;

        if token.kind() != range_start_token.kind() {
            return Err(ParseError::ExpectedOtherToken.with2(format!("expected {:?}", range_start_token.kind()), token.text_range()));
        }
        let range_end_token = token.clone();

        token = parsing::expect_next(token)?;
        parsing::consume_expected_list_forward(token, &[
            Expected::Optional(Box::new(Expected::Kind(SyntaxKind::WHITESPACE))),
            Expected::Kind(SyntaxKind::R_PAREN),
            Expected::Optional(Box::new(Expected::Kind(SyntaxKind::WHITESPACE))),
            Expected::Kind(SyntaxKind::SEMICOLON),
        ])?;

        *remaining = &remaining[1..];

        let items = parse_items(data, remaining, None, ParseItemsEnd::EndMacro { start: expr_start_token })?;

        Ok(Expr::ForRange {
            ident: ident_text,
            range: Range::Numeric {
                from: range_start_token.text().parse().unwrap(),
                to: range_end_token.text().parse().unwrap()
            },
            items,
        })
    }

    pub fn is_expr_start(ident_token: &SyntaxToken) -> bool {
        assert_eq!(ident_token.kind(), SyntaxKind::IDENT);
        let text = ident_token.text().as_str();
        text == "for_range"
            || text == "end"
    }

    pub fn write(&self, data: &str, tmp_data: Option<&str>, generation_point: &GenerationPoint, output: &mut impl Write, stack: &mut Stack, parent_ident: Option<&str>) -> Result<(), ParseErrorWithPos> {
        match self {
            Expr::ForRange { range: Range::Numeric { from, to }, items, ident } => {
                stack.push_named_value(ident, Varying::Integer(*from));
                for i in *from..*to {
                    stack.update_named_value(ident, Varying::Integer(i));
                    for item in items {
                        item.write(data, tmp_data, generation_point, output, stack, parent_ident)?;
                    }
                }
                stack.pop_named_value(ident);
            },
        }

        Ok(())
    }
}

pub enum Item {
    TemplateInvocation(TemplateInvocation),
    Expr(Expr),
    Content(String),
    Block { items: Vec<Item>, ident: Option<String> },
}

impl Item {
    pub fn parse_from_macro(data: &str, remaining: &mut &[ReltNode]) -> Result<Item, ParseErrorWithPos> {
        let macro_call = match remaining.first().unwrap() {
            ReltNode::TemplateInvocation(mc) => mc,
            _ => unreachable!("expected ReltNode::TemplateInvocation"),
        };

        let mut token = macro_call.syntax().first_token().unwrap();
        while token.kind() != SyntaxKind::IDENT {
            token = parsing::expect_next(token)?;
        }

        if Expr::is_expr_start(&token) {
            trace!("parse as expr {:?}", token);
            return Ok(Item::Expr(Expr::parse_from_macro(data, remaining, token)?));
        }

        trace!("parse as template invocation {:?}", token);

        Ok(Item::TemplateInvocation(TemplateInvocation::parse(token)?))
    }

    pub fn write(&self, data: &str, tmp_data: Option<&str>, generation_point: &GenerationPoint, output: &mut impl Write, stack: &mut Stack, parent_ident: Option<&str>) -> Result<(), ParseErrorWithPos> {
        match self {
            Item::TemplateInvocation(inv) => {
                match generation_point.find_template(&inv.name, data, tmp_data) {
                    None => unreachable!("template not located by name {}", inv.name),
                    Some((template, template_data)) => {
                        write!(output, "\n{}", inv.produce(data, template_data, template, &*stack, parent_ident)?).unwrap()
                    },
                }
            },
            Item::Content(c) => {
                // if let Some(parent_ident) = parent_ident {
                //     write!(output, "{}{}", parent_ident, c).unwrap()
                // } else {
                    write!(output, "{}", c).unwrap()
                // }
            },
            Item::Expr(e) => e.write(data, tmp_data, generation_point, output, stack, parent_ident)?,
            Item::Block { items, ident } => {
                for item in items {
                    item.write(data, tmp_data, generation_point, output, stack, ident.as_ref().map(|s| &s[..]))?;
                }
            }
        }

        Ok(())
    }
}

#[derive(Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
pub struct ModPath(String);

impl AsRef<str> for ModPath {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl ModPath {
    pub fn from_generator_path(path: &RelativePath, config: ParseConfig) -> ModPath {
        let parent = path.parent().unwrap();
        let file_name = path.file_name().unwrap();
        let file_name_stripped = file_name.strip_suffix(config.gen_file_suffix).unwrap();
        let mut path = parent.components().map(|c| c.as_str()).join("::");
        path.push_str("::");
        path.push_str(file_name_stripped);
        ModPath(path)
    }
}

pub struct GenerationPoint {
    pub generator: GenerationFile,
    pub template: Option<TemplateFile>,
}

pub struct GenerationFile {
    pub relative_path: RelativePathBuf,
    pub items: Vec<Item>,
    pub templates: HashMap<String, Template>,
}

pub struct TemplateFile {
    pub relative_path: RelativePathBuf,
    pub templates: HashMap<String, Template>,
}

impl GenerationPoint {
    pub fn find_template<'q, 'p>(&'p self, name: &str, data: &'q str, tmp_data: Option<&'q str>) -> Option<(&'p Template, &'q str)> {
        if let Some(t) = self.generator.templates.get(name) {
            return Some((t, data));
        }

        if let (&Some(ref tf), Some(data)) = (&self.template, tmp_data) {
            if let Some(t) = tf.templates.get(name) {
                return Some((t, data));
            }
        }

        None
    }

    fn parse_additional_templates(relative_path: &RelativePath, data: &str) -> Result<HashMap<String, Template>, ParseErrorWithPosAndFile> {
        let parsed = syntax::SourceFile::parse(&data);

        trace!("error count {:?} {:?}", relative_path, parsed.errors().len());

        for e in parsed.errors() {
            return Err(
                ParseError::SyntaxError
                    .with2(e.to_string(), e.range())
                    .with_file(relative_path)
            );
        }

        let tree = parsed.tree();

        let mut templates = HashMap::new();
        let items: Vec<ReltNode> = relt::collect(tree);
        let mut remaining_items: &[ReltNode] = &items;

        parse_only_templates(&mut remaining_items, &mut templates).map_err(|e| e.with_file(relative_path))?;

        Ok(templates)
    }

    pub fn parse(relative_path: &RelativePath, data: &str, tmp_relative_path: Option<&RelativePath>, tmp_data: Option<&str>) -> Result<GenerationPoint, ParseErrorWithPosAndFile> {
        let parsed = syntax::SourceFile::parse(&data);

        trace!("error count {:?} {:?}", relative_path, parsed.errors().len());

        for e in parsed.errors() {
            return Err(
                ParseError::SyntaxError
                    .with2(e.to_string(), e.range())
                    .with_file(relative_path)
            );
        }

        let tree = parsed.tree();
        let mut templates = HashMap::new();
        let items: Vec<ReltNode> = relt::collect(tree);
        let mut remaining_items: &[ReltNode] = &items;

        Ok(GenerationPoint {
            template: if let (Some(tmp_relative_path), Some(tmp_data)) = (tmp_relative_path, tmp_data) {
                Some(TemplateFile {
                    templates: Self::parse_additional_templates(tmp_relative_path, tmp_data)?,
                    relative_path: tmp_relative_path.to_relative_path_buf()
                })
            } else {
                None
            },
            generator: GenerationFile {
                relative_path: relative_path.to_owned(),
                items: parse_items(data, &mut remaining_items, Some(&mut templates), ParseItemsEnd::EndOfItems).map_err(|e| e.with_file(relative_path))?,
                templates,
            }
        })
    }

    pub fn write(&self, data: &str, tmp_data: Option<&str>, output: &mut impl Write, stack: &mut Stack, parent_ident: Option<&str>) -> Result<(), ParseErrorWithPos> {
        for item in self.generator.items.iter() {
            item.write(data, tmp_data, &self, output, stack, parent_ident)?;
        }
        Ok(())
    }
}

#[derive(Eq, PartialEq)]
enum ParseItemsEnd {
    EndOfItems,
    EndMacro { start: SyntaxToken },
}

fn parse_only_templates(remaining_items: &mut &[ReltNode], templates: &mut HashMap<String, Template>) -> Result<(), ParseErrorWithPos> {
    trace!("==> parse_templates remaining={}", remaining_items.len());

    while remaining_items.len() > 0 {
        match remaining_items.first().unwrap() {
            ReltNode::Template(ref macro_call) => {
                trace!("parse as template {:?}", macro_call);
                match Template::parse(&macro_call) {
                    Ok(template) => {
                        templates.insert(template.name.clone(), template);
                    },
                    Err(e) => {
                        return Err(
                            ParseError::SyntaxError
                                .with2(e.message, e.range)
                        );
                    },
                }
            },
            ReltNode::TemplateInvocation(macro_call) => {
                return Err(ParseError::ExpectedOtherToken
                    .with("only \"macro_rules\" macro is allowed inside a template file", macro_call.syntax().text_range()));
            },
            ReltNode::Mod(relt::ReltMod { syntax, .. }) => {
                return Err(ParseError::ExpectedOtherToken
                    .with("only \"macro_rules\" macro is allowed inside a template file", syntax.text_range()));
            },
            ReltNode::OtherItem(other) => {
                return Err(ParseError::ExpectedOtherToken
                    .with("only \"macro_rules\" is allowed inside a template file", other.text_range()));
            },
        }

        *remaining_items = &remaining_items[1..];
    }

    trace!("<== parse_templates remaining={}", remaining_items.len());

    Ok(())
}

fn parse_items(data: &str, remaining_items: &mut &[ReltNode], mut templates: Option<&mut HashMap<String, Template>>, end_type: ParseItemsEnd) -> Result<Vec<Item>, ParseErrorWithPos> {
    trace!("==> parse_items remaining={}", remaining_items.len());

    let mut output_items = Vec::new();

    enum State {
        Done,
        NotMacroRulesButMacro,
        NotMacro,
        Module,
    }

    while if end_type == ParseItemsEnd::EndOfItems {
        remaining_items.len() > 0
    } else {
        remaining_items.len() > 0 && !item_macro_name_equals(&(remaining_items[0]), "end")
    } {
        let state: State;

        match remaining_items.first().unwrap() {
            ReltNode::Template(macro_call) => {
                if let Some(ref mut templates) = templates {
                    trace!("parse as template {:?}", macro_call);
                    match Template::parse(&macro_call) {
                        Ok(template) => {
                            templates.insert(template.name.clone(), template);
                        },
                        Err(e) => {
                            return Err(
                                ParseError::SyntaxError
                                    .with2(e.message, e.range)
                            );
                        },
                    }
                    state = State::Done;
                } else {
                    return Err(ParseError::ExpectedOtherToken.with("macro_rules is not allowed inside blocks", macro_call.syntax().text_range()));
                }
            },
            ReltNode::TemplateInvocation(_) => {
                state = State::NotMacroRulesButMacro;
            },
            ReltNode::Mod(_) => {
                state = State::Module;
            }
            _ => {
                state = State::NotMacro;
            },
        }

        match state {
            State::Done => {},
            State::NotMacroRulesButMacro => {
                trace!("parse as item {:?}", remaining_items[0].syntax());
                output_items.push(Item::parse_from_macro(data,remaining_items)?);
            },
            State::Module => {
                if let ReltNode::Mod(module) = remaining_items.first().unwrap() {
                    trace!("parse as mod {:?}", module.syntax);
                    let (start_string, ident) = module.start_string_and_indent(data)?;
                    output_items.push(Item::Content(start_string));
                    output_items.push(Item::Block { items: parse_items(data, &mut &module.items[..], None, ParseItemsEnd::EndOfItems)?, ident });
                    output_items.push(Item::Content(module.end_string(data)?));
                }
            }
            State::NotMacro => {
                let it = remaining_items.first().unwrap();
                if let Some(token) = it.syntax().first_token() {
                    let ws_token = if token.kind() == SyntaxKind::WHITESPACE {
                        Some(token)
                    } else {
                        if let Some(prev_token) = token.prev_token() {
                            if prev_token.kind() == SyntaxKind::WHITESPACE {
                                Some(prev_token)
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    };

                    if let Some(ws_token) = ws_token {
                        let ws_start = u32::from(ws_token.text_range().start()) as usize;
                        let ws_end = u32::from(ws_token.text_range().end()) as usize;
                        let ident = String::from_utf8_lossy(&data.as_bytes()[
                            ws_start .. ws_end
                            ]).into_owned();
                        output_items.push(Item::Content(ident));
                    }

                    trace!("output content {:?}", it.syntax());
                    output_items.push(Item::Content(it.syntax().to_string()));
                }
            },
        }

        *remaining_items = &remaining_items[1..];
    }

    match (remaining_items.len(), end_type) {
        (0, ParseItemsEnd::EndMacro { start }) =>
            return Err(ParseError::ExpectedNextToken.with("missing end!", start.text_range())),
        (_, ParseItemsEnd::EndMacro { .. }) => {
            let token = remaining_items.first().unwrap().syntax().first_token().unwrap();
            parsing::consume_expected_list_forward(token, &[
                Expected::Optional(Box::new(Expected::Kind(SyntaxKind::COMMENT))),
                Expected::Optional(Box::new(Expected::Kind(SyntaxKind::WHITESPACE))),
                Expected::Kind(SyntaxKind::IDENT),
                Expected::Kind(SyntaxKind::BANG),
                Expected::Optional(Box::new(Expected::Kind(SyntaxKind::WHITESPACE))),
                Expected::Kind(SyntaxKind::L_PAREN),
                Expected::Optional(Box::new(Expected::Kind(SyntaxKind::WHITESPACE))),
                Expected::Kind(SyntaxKind::R_PAREN),
                Expected::Optional(Box::new(Expected::Kind(SyntaxKind::WHITESPACE))),
            ])?;
        },
        _ => (),
    };

    trace!("<== parse_items remaining={}", remaining_items.len());

    Ok(output_items)
}

pub struct Template {
    pub name: String,
    pub args: Vec<(TextRange, String)>,
    pub template_start: usize,
    pub template_end: usize,
}

impl Template {
    pub fn parse(macro_rules: &ast::MacroRules) -> Result<Template, ParseErrorWithPos> {
        let mut start_token = macro_rules.syntax().first_token().unwrap();
        while start_token.kind() != SyntaxKind::IDENT {
            start_token = parsing::expect_next(start_token)?;
        }

        let mut end_token = macro_rules.syntax().last_token().unwrap();

        let (next_token, _) = parsing::expect_any(start_token, &[
            Expected::Kind(SyntaxKind::IDENT),
        ])?;

        let name = next_token.text().to_string();
        start_token = parsing::expect_next(next_token)?;

        debug!("name {:?}", name);

        start_token = parsing::consume_expected_list_forward(start_token, &[
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
        for (range, _, replacement) in self.lookup_args() {
            if !template.contains(&replacement) {
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

    pub fn lookup_args<'q>(&'q self) -> impl Iterator<Item=(TextRange, &'q str, String)> + 'q {
        self.args.iter()
            .map(move |(range, arg_name)| (range.clone(), &arg_name[..], format!("<{}>", arg_name)))
    }
}

pub struct TemplateInvocation {
    pub name: String,
    pub args: Vec<Vec<SyntaxToken>>,
    pub name_range: TextRange,
    pub arg_range: TextRange,
}

impl TemplateInvocation {
    pub fn parse(ident_token: SyntaxToken) -> Result<TemplateInvocation, ParseErrorWithPos> {
        assert_eq!(ident_token.kind(), SyntaxKind::IDENT);

        let name = ident_token.text().to_string();
        let name_range = ident_token.text_range();
        trace!("name {:?}", name);

        let mut token = parsing::expect_next(ident_token)?;

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
            let mut arg_tokens: Vec<SyntaxToken> = Vec::new();

            'consume: loop {
                if token.kind() == SyntaxKind::COMMA {
                    if arg_tokens.len() > 0 {
                        args.push(arg_tokens);
                        token = parsing::expect_next(token)?;
                        break 'consume;
                    } else {
                        return Err(ParseError::ExpectedOtherToken.with(
                            "expected tokens for argument",
                            TextRange::new(start_token.text_range().start(), last_token.text_range().end())
                        ));
                    }
                } else if token.kind() == SyntaxKind::R_PAREN {
                    if arg_tokens.len() > 0 {
                        args.push(arg_tokens);
                    }
                    args_end = token.prev_token().unwrap().text_range().end();
                    token = parsing::expect_next(token)?;
                    break 'main;
                } else if token.kind() == SyntaxKind::WHITESPACE {
                } else {
                    arg_tokens.push(token.clone());
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

    pub fn produce(&self, inv_data: &str, template_data: &str, template: &Template, stack: &Stack, parent_ident: Option<&str>) -> Result<String, ParseErrorWithPos> {
        let input_bytes = template.get_bytes(template_data);
        let (bytes, start_newline) = if input_bytes.starts_with(b"\n") {
            (&input_bytes[1..], &input_bytes[..1])
        } else if input_bytes.starts_with(b"\r\n") {
            (&input_bytes[2..], &input_bytes[..2])
        } else {
            (input_bytes, &b""[..])
        };

        // you get your indent removed if it is consistent with the first line
        let mut whitespace_ident = Vec::new();
        for b in bytes {
            if !(b.is_ascii_whitespace() && *b != b'\r' && *b != b'\n') {
                break;
            }
            whitespace_ident.push(*b);
        }

        let mut adjusted_template = String::from_utf8_lossy(start_newline).into_owned();
        for line in LinesIter::new(bytes) {
            if let Some(parent_ident) = parent_ident {
                adjusted_template.push_str(parent_ident);
            }

            if line.starts_with(&whitespace_ident[..]) {
                adjusted_template.push_str(std::str::from_utf8(&line[whitespace_ident.len()..]).expect("line from utf bytes"));
            } else {
                adjusted_template.push_str(std::str::from_utf8(&line).expect("line from utf bytes 2"));
            }
        }

        let mut replacements = HashMap::new();
        for ((_param_range, _parameter, search), arg_value) in template.lookup_args().zip(self.args.iter()) {
            if let (2, SyntaxKind::BANG) = (arg_value.len(), arg_value[0].kind()) {
                let ident_arg = arg_value.get(1).expect("validated to have 2 tokens");
                let arg_name = ident_arg.to_string();
                if let Some(value) = stack.get_named(&arg_name) {
                    replacements.insert(search, value.to_string());
                    continue;
                } else {
                    return Err(ParseError::RuntimeError.with2(format!("named argument {:?} is not in scope", arg_name), ident_arg.text_range()));
                }
            }

            let value_text = &inv_data.as_bytes()[u32::from(arg_value[0].text_range().start()) as usize..u32::from(arg_value[arg_value.len() - 1].text_range().end()) as usize];
            replacements.insert(search, String::from_utf8_lossy(value_text).into_owned());
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

        Ok(final_template)
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

fn item_macro_name_equals(item: &ReltNode, value: &str) -> bool {
    if let ReltNode::TemplateInvocation(ref macro_call) = item {
        if let Some(mut token) = macro_call.syntax().first_token() {
            while token.kind() != SyntaxKind::IDENT {
                token = match token.next_token() {
                    None => return false,
                    Some(token) => token,
                };
            }

            if token.text() == value {
                return true;
            }
        }
    }

    false
}