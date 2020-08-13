use ra_syntax::{SyntaxToken, SyntaxKind, TextRange};
use relative_path::{RelativePathBuf, RelativePath};

pub fn expect_any(mut token: SyntaxToken, one_of: &[Expected]) -> Result<(SyntaxToken, usize), ParseErrorWithPos> {
    let optional_list: Vec<_> = one_of.iter().filter_map(|item| match item {
        Expected::Optional(opt) => Some(opt),
        _ => None,
    }).collect();

    let not_optional_list: Vec<_> = one_of.iter().enumerate().filter_map(|(i, item)| match item {
        Expected::Optional(_) => None,
        o => Some((i, o)),
    }).collect();

    if optional_list.len() > 0 {
        loop {
            let mut matches = false;
            for optional in optional_list.iter() {
                if let Ok(()) = optional.match_it(&token) {
                    matches = true;
                    break;
                }
            }

            if matches {
                token = expect_next(token)?;
            } else {
                break;
            }
        }
    }

    for (i, expected) in not_optional_list.iter() {
        if let Ok(()) = expected.match_it(&token) {
            return Ok((token, *i));
        }
    }

    let mut message = String::new();
    if one_of.len() == 1 {
        message.push_str("expected ");
        message.push_str(&one_of.get(0).unwrap().info());
    } else {
        message.push_str("expected ");
        for (i, e) in one_of.iter().enumerate() {
            if i > 0 {
                if i < one_of.len() - 1 {
                    message.push_str(", ");
                } else {
                    message.push_str(" or ");
                }
            }
            message.push_str(&e.info());
        }
    }

    Err(ParseError::ExpectedOtherToken.with2(message, token.text_range()))
}

pub fn consume_expected_list_forward(mut token: SyntaxToken, expected_list: &[Expected]) -> Result<SyntaxToken, ParseErrorWithPos> {
    for expected in expected_list {
        match expected.match_it(&token) {
            Ok(_) => token = expect_next(token)?,
            Err(None) => (),
            Err(Some(e)) => return Err(e),
        }
    }

    Ok(token)
}

pub fn consume_expected_list_backward(mut token: SyntaxToken, expected_list: &[Expected]) -> Result<SyntaxToken, ParseErrorWithPos> {
    for expected in expected_list {
        match expected.match_it(&token) {
            Ok(_) => token = expect_previous(token)?,
            Err(None) => (),
            Err(Some(e)) => return Err(e),
        }
    }

    Ok(token)
}

pub fn expect_next(mut token: SyntaxToken) -> Result<SyntaxToken, ParseErrorWithPos> {
    token = token.next_token().ok_or(
        ParseError::ExpectedOtherToken
            .with("expected next token",token.text_range())
    )?;
    Ok(token)
}

pub fn expect_previous(mut token: SyntaxToken) -> Result<SyntaxToken, ParseErrorWithPos> {
    token = token.prev_token().ok_or(
        ParseError::ExpectedOtherToken
            .with("expected previous token",token.text_range())
    )?;
    Ok(token)
}

pub enum Expected<'a> {
    Kind(SyntaxKind),
    KindAndText(SyntaxKind, &'a str),
    Text(&'a str),
    Optional(Box<Expected<'a>>),
}

impl<'a> Expected<'a> {
    pub fn match_it(&self, token: &SyntaxToken) -> Result<(), Option<ParseErrorWithPos>> {
        match self {
            Expected::Kind(kind) => if !(token.kind() == *kind) {
                return Err(Some(
                    ParseError::ExpectedOtherToken
                        .with2(format!("expected {:?}", kind),token.text_range())
                ));
            },
            Expected::KindAndText(kind, text) => if !(token.kind() == *kind && token.text().eq(text)) {
                return Err(Some(
                    ParseError::ExpectedOtherToken
                        .with2(format!("expected {:?} {}", kind, text),token.text_range())
                ));
            },
            Expected::Text(text) => if !(token.text() == text) {
                return Err(Some(
                    ParseError::ExpectedOtherToken
                        .with2(format!("expected {}", text),token.text_range())
                ));
            },
            Expected::Optional(exp) => {
                if let Err(Some(_)) = exp.match_it(token) {
                    return Err(None);
                }
            }
        }

        Ok(())
    }

    pub fn info(&self) -> String {
        match self {
            Expected::Kind(kind) => format!("{:?}", kind),
            Expected::KindAndText(kind, text) => format!("{:?} {}", kind, text),
            Expected::Text(text) => format!("{}", text),
            Expected::Optional(e) => e.info(),
        }
    }
}

pub enum ParseError {
    SyntaxError,
    TypeError,
    ExpectedNextToken,
    ExpectedOtherToken,
}

impl ParseError {
    pub fn with(self, message: &str, range: TextRange) -> ParseErrorWithPos {
        ParseErrorWithPos {
            e: self,
            message: message.into(),
            range,
        }
    }

    pub fn with2(self, message: String, range: TextRange) -> ParseErrorWithPos {
        ParseErrorWithPos {
            e: self,
            message,
            range,
        }
    }
}

pub struct ParseErrorWithPosAndFile {
    pub e: ParseError,
    pub message: String,
    pub range: TextRange,
    pub file: RelativePathBuf,
}

pub struct ParseErrorWithPos {
    pub e: ParseError,
    pub message: String,
    pub range: TextRange,
}

impl ParseErrorWithPos {
    pub fn with_file(self, relative_path: &RelativePath) -> ParseErrorWithPosAndFile {
        ParseErrorWithPosAndFile {
            e: self.e,
            message: self.message,
            range: self.range,
            file: relative_path.to_owned(),
        }
    }
}