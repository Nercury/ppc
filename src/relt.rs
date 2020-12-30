//! Relevant parsed tree

use syntax::{ast, SyntaxNode, AstNode, SourceFile, SyntaxKind};
use syntax::ast::ModuleItemOwner;
use crate::parsing;
use crate::parsing::{ParseErrorWithPos};

/// The ast tree sections wrapped into another enum that makes sense for preprocessor
pub enum ReltNode {
    Template(ast::MacroRules),
    TemplateInvocation(ast::MacroCall),
    OtherItem(SyntaxNode),
    Mod(ReltMod),
}

impl ReltNode {
    pub fn from_item(item: ast::Item) -> ReltNode {
        match item {
            ast::Item::MacroRules(mc) => {
                ReltNode::Template(mc)
            },
            ast::Item::MacroCall(mc) => {
                ReltNode::TemplateInvocation(mc)
            },
            ast::Item::Module(m) => ReltNode::Mod(ReltMod::from_module_ast(m)),
            ref other => ReltNode::OtherItem(other.syntax().clone()),
        }
    }

    pub fn syntax(&self) -> &SyntaxNode {
        match self {
            ReltNode::Template(t) => t.syntax(),
            ReltNode::TemplateInvocation(t) => t.syntax(),
            ReltNode::OtherItem(s) => s,
            ReltNode::Mod(ReltMod { syntax, .. }) => syntax,
        }
    }
}

pub struct ReltMod {
    pub syntax: SyntaxNode,
    pub items: Vec<ReltNode>,
}

impl ReltMod {
    pub fn from_module_ast(node: ast::Module) -> ReltMod {
        let items = if let Some(list) = node.item_list() {
            list.items()
                .map(|item| ReltNode::from_item(item))
                .collect()
        } else {
            Vec::new()
        };

        ReltMod {
            syntax: node.syntax().clone(),
            items,
        }
    }

    pub fn start_string_and_indent(&self, data: &str) -> Result<(String, Option<String>), ParseErrorWithPos> {
        Ok(if let Some(mut token) = self.syntax.first_token() {
            let mut start = u32::from(token.text_range().start()) as usize;
            if let Some(previous_token) = token.prev_token() {
                if previous_token.kind() == SyntaxKind::WHITESPACE {
                    start = u32::from(previous_token.text_range().start()) as usize;
                }
            }

            while token.kind() != SyntaxKind::L_CURLY {
                token = parsing::expect_next(token)?;
            }
            let curly_end = u32::from(token.text_range().end()) as usize;

            token = parsing::expect_next(token)?;
            let mut ident = None;
            if token.kind() == SyntaxKind::WHITESPACE {
                let ws_start = u32::from(token.text_range().start()) as usize;
                let ws_end = u32::from(token.text_range().end()) as usize;

                let ws_range = &data.as_bytes()[ws_start..ws_end];

                if let Some((pos, _)) = ws_range
                    .iter()
                    .enumerate()
                    .rev()
                    .filter(|(_, b)| **b == b'\n')
                    .next() {

                    let ident_start = ws_start + pos + 1;

                    ident = Some(String::from_utf8_lossy(&data.as_bytes()[
                        ident_start .. ws_end
                    ]).into_owned())
                }
            }

            debug!("ident {:?}", ident);

            (String::from_utf8_lossy(&data.as_bytes()[
                start .. curly_end
            ]).into_owned(), ident)
        } else {
            (String::new(), None)
        })
    }

    pub fn end_string(&self, data: &str) -> Result<String, ParseErrorWithPos> {
        Ok(if let Some(mut token) = self.syntax.last_token() {
            let end = token.text_range().end();

            while token.kind() != SyntaxKind::R_CURLY {
                token = parsing::expect_previous(token)?;
            }

            let mut start = u32::from(token.text_range().start()) as usize;
            if let Some(previous_token) = token.prev_token() {
                if previous_token.kind() == SyntaxKind::WHITESPACE {
                    start = u32::from(previous_token.text_range().start()) as usize;
                    let ws_end = u32::from(token.text_range().start()) as usize;

                    let ws_range = &data.as_bytes()[start..ws_end];
                    if let Some((pos, _)) = ws_range
                        .iter()
                        .enumerate()
                        .filter(|(i, b)|
                            **b == b'\n'
                                || (**b == b'\r' && *i + 1 < ws_range.len() && ws_range[i + 1] == b'\n') // or windows
                        ).next() {
                        start += pos;
                    }
                }
            }

            String::from_utf8_lossy(&data.as_bytes()[
                start .. u32::from(end) as usize
            ]).into_owned()
        } else {
            String::new()
        })
    }
}

pub fn collect(source_file: SourceFile) -> Vec<ReltNode> {
    source_file
        .items()
        .map(|item| ReltNode::from_item(item))
        .collect()
}