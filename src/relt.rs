//! Relevant parsed tree

use ra_syntax::{ast, SyntaxNode, AstNode, SourceFile};
use ra_syntax::ast::ModuleItemOwner;

/// The ast tree sections wrapped into another enum that makes sense for preprocessor
pub enum ReltNode {
    Template(ast::MacroCall),
    TemplateInvocation(ast::MacroCall),
    OtherItem(SyntaxNode),
}

impl ReltNode {
    pub fn from_item(item: ast::Item) -> ReltNode {
        match item {
            ast::Item::MacroCall(mc) => if let Some(_) = mc.is_macro_rules() {
                ReltNode::Template(mc)
            } else {
                ReltNode::TemplateInvocation(mc)
            },
            // ast::Item::Module(_) => {},
            ref other => ReltNode::OtherItem(other.syntax().clone()),
        }
    }

    pub fn syntax(&self) -> &SyntaxNode {
        match self {
            ReltNode::Template(t) => t.syntax(),
            ReltNode::TemplateInvocation(t) => t.syntax(),
            ReltNode::OtherItem(s) => s,
        }
    }
}

pub fn collect(source_file: SourceFile) -> Vec<ReltNode> {
    source_file
        .items()
        .map(|item| ReltNode::from_item(item))
        .collect()
}