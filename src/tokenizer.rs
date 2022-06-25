use std::{borrow::Cow, ops::Range};

use logos::{FilterResult, Logos};
use rustc_lexer::unescape::{unescape_raw_str, unescape_str, EscapeError};
use unicode_normalization::UnicodeNormalization;

pub type Lexer<'source> = logos::Lexer<'source, Token<'source>>;
pub type TokenStream<'source> = &'source mut TokenBuffer<'source>;
// #[macro_export]
// macro_rules! TokenStream {
//     () => {
//         std::iter::Peekable<impl Iterator<Item = (Token<'_>, Range<usize>)>>
//     };
//     ($lt:lifetime) => {
//         std::iter::Peekable<impl Iterator<Item = (Token<$lt>, Range<usize>)>>
//     }
// }
pub struct TokenBuffer<'source> {
    tokens: Vec<(Token<'source>, Range<usize>)>,
    pub position: usize,
}
impl<'source> TokenBuffer<'source> {
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Option<&(Token<'source>, Range<usize>)> {
        if self.position == self.tokens.len() {
            None
            // &(Token::EOF, end..end)
        } else {
            self.position += 1;
            Some(&self.tokens[self.position])
        }
    }

    pub fn peek(&self) -> Option<&(Token, Range<usize>)> {
        self.tokens.get(self.position + 1)
    }

    pub fn current(&self) -> usize {
        self.position
    }

    pub fn reset_to(&mut self, position: usize) {
        self.position = position
    }
}

impl<'source> FromIterator<(Token<'source>, Range<usize>)> for TokenBuffer<'source> {
    fn from_iter<T: IntoIterator<Item = (Token<'source>, Range<usize>)>>(iter: T) -> Self {
        TokenBuffer {
            tokens: iter.into_iter().collect(),
            position: 0,
        }
    }
}

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token<'source> {
    // TODO Only normalize if necessary
    #[regex(r"(\p{XID_START}|_)\p{XID_CONTINUE}*", |l|l.slice().nfc().collect::<Cow<str>>())]
    Ident(Cow<'source, str>),

    // Keywords
    #[token("fn")]
    Fn,
    #[token("hid")]
    Hid,
    #[token("pub")]
    Pub,
    #[token("pre")]
    Pre,
    #[token("post")]
    Post,

    // Literals
    #[regex(r#""(\\\\|\\"|[^"])*""#, string)]
    #[regex(r#"'(\\\\|\\'|[^'])*'"#, string)]
    String(Cow<'source, str>),

    #[regex(r#"r#*""#, raw_string)]
    #[regex(r#"r#*'"#, raw_string)]
    RawString(Cow<'source, str>),

    #[regex(r"0[xob]_*[0-9][0-9_]*", prefixed_int)]
    #[regex(r"[0-9][0-9_]*(.[0-9][0-9_]*)?([Ee][+-]?_*[0-9][0-9]*)?", |lex|lex.slice().replace('_', "").parse())]
    Number(f64),

    // brackets
    #[token("{")]
    BraceOpen,
    #[token("}")]
    BraceClose,
    #[token("(")]
    ParenOpen,
    #[token(")")]
    ParenClose,
    #[token("[")]
    BracketOpen,
    #[token("]")]
    BracketClose,

    // Symbols
    #[token(";")]
    Semi,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token("..")]
    RangeSep,
    #[token("..=")]
    InklRangeSep,
    #[token("?")]
    Question,
    #[token(":")]
    Colon,
    #[token("::")]
    PathSep,
    #[token("=")]
    Assign,
    #[token("==")]
    Eq,
    #[token("!=")]
    NotEq,
    #[token("!")]
    Bang,
    #[token("<")]
    Lt,
    #[token("<=")]
    LtEq,
    #[token(">")]
    Gt,
    #[token(">=")]
    GtEq,
    #[token("->")]
    Arrow,
    #[token("-")]
    Minus,
    #[token("&&")]
    And,
    #[token("&&=")]
    AndAssign,
    #[token("||")]
    Or,
    #[token("||=")]
    OrAssign,
    #[token("+")]
    Plus,
    #[token("+=")]
    PlusAssign,
    #[token("*")]
    Mul,
    #[token("*=")]
    MulAssign,
    #[token("/")]
    Div,
    #[token("/=")]
    DivAssign,
    #[token("^")]
    Exp,
    #[token("^=")]
    ExpAssign,
    #[token("%")]
    Mod,
    #[token("%=")]
    ModAssign,

    #[regex(r"\|(o|e|oe|eo)>")]
    Pipe,

    #[regex(r"///([^/\n][^\n]*)?\n", doc_comment)]
    DocComment(&'source str),

    // Ignored and errors
    #[error]
    #[regex(r"\p{Whitespace}", logos::skip)]
    #[regex(r"(//|////)[^\n]*\n", logos::skip)]
    #[regex(r"#![^\n]*\n", |l| if l.span().start == 0 {FilterResult::Skip} else {FilterResult::Error})]
    Error,

    EOF,
}

fn doc_comment<'source>(l: &mut Lexer<'source>) -> &'source str {
    let comment = l.slice().trim_start_matches("///").trim_end();
    comment.strip_prefix(' ').unwrap_or(comment)
}

// TODO should be Cow
fn string<'source>(l: &mut Lexer<'source>) -> Cow<'source, str> {
    let string = l.slice();
    let str = string.get(1..string.len() - 1).expect("quotes are ascii");
    let mut last_end = 0;
    let mut string = String::new();

    unescape_str(str, &mut |span, res| match res {
        Ok(c) => {
            string.push_str(str.get(last_end..span.start).expect("is in range"));
            string.push(c);
            last_end = span.end;
        }
        Err(EscapeError::EscapeOnlyChar) if str.get(span.start..span.end) == Some("\"") => (),
        Err(e) => todo!("Handle String escape errors{e:?}"),
    });

    string.push_str(
        str.get(last_end..str.len())
            .expect("last_end is <= str.len()"),
    );

    string.into()
}

fn raw_string<'source>(l: &mut Lexer<'source>) -> Cow<'source, str> {
    let string = l.slice();
    let quote = string
        .get(string.len() - 1..string.len())
        .expect("there is a quote");
    let pounds = string.len() - 2;
    let end = quote.to_owned()
        + string
            .get(1..string.len() - 1)
            .expect("pounds inside string");
    let quote = quote.chars().next().expect("there is a quote");

    let remainder = l.remainder();

    let mut string = None;
    for (i, c) in remainder.char_indices() {
        if c == quote
            && remainder.len() > i + pounds + 1
            && remainder
                .get(i..i + pounds + 1)
                .expect("i stays in bounds - pounds +1 ")
                == end
        {
            string = Some(remainder.get(0..i).expect("i is in bounds"));
            l.bump(i + pounds + 1);
            break;
        }
    }

    // TODO error handling
    let str = string.expect("found end");

    let mut last_end = 0;
    let mut string = String::new();

    unescape_raw_str(str, &mut |span, res| match res {
        Ok(c) => {
            string.push_str(str.get(last_end..span.start).expect("is in range"));
            string.push(c);
            last_end = span.end;
        }
        Err(e) => todo!("Handle String escape errors{e:?}"),
    });

    string.push_str(
        str.get(last_end..str.len())
            .expect("last_end is <= str.len()"),
    );

    string.into()
}

fn prefixed_int(l: &mut Lexer) -> Result<f64, std::num::ParseIntError> {
    let base = l.slice().get(1..2).expect("has base");
    let remainder = l
        .slice()
        .get(2..l.slice().len())
        .expect("has number")
        .replace('_', "");
    let base = match base.to_ascii_lowercase().as_str() {
        "o" => 8,
        "x" => 16,
        "b" => 2,
        _ => unreachable!("only oxb are bases"),
    };

    i64::from_str_radix(&remainder, base).map(|i| i as f64)
}

impl<'source> PartialEq<&str> for Token<'source> {
    fn eq(&self, string: &&str) -> bool {
        match self {
            Self::Ident(ident) => ident == string,
            _ => false,
        }
    }
}

pub fn tokenize(input: &str) -> TokenBuffer {
    Token::lexer(input).spanned().collect()
}

#[cfg(test)]
mod test {
    use crate::tokenizer::{Logos, Token};

    macro_rules! token_test {
        // {$tokens:ident => $token:ident($ident:tt) $($tts:tt)*} => {
        //     assert_eq!($tokens.next().unwrap(), Token::$token($ident.into()));
        //     token_test!{$tokens => $($tts)*}
        // };
        {$tokens:ident => $token:ident($($inner:tt),*) $($tts:tt)*} => {
            assert_eq!($tokens.next().unwrap(), Token::$token($($inner.into()),*),"`{}` at {:?}", $tokens.slice(), $tokens.span());
            token_test!{$tokens => $($tts)*}
        };
        {$tokens:ident => $token:ident $($tts:tt)*} => {
            assert_eq!($tokens.next().unwrap(), Token::$token,"`{}` at {:?}", $tokens.slice(), $tokens.span());
            token_test!{$tokens => $($tts)*}
        };
        {$tokens:ident =>} => {}
    }

    #[test]
    fn test() {
        let mut tokens = Token::lexer(include_str!("../test/tokenize.fns"));
        token_test! { tokens =>
            DocComment("Doc comment")
            DocComment("  - This should be indented")
            Ident("pre") Ident("fn") ParenOpen ParenClose BraceOpen
                Ident("pub") Ident("let") Ident("config") Colon BraceOpen
                    Ident("source") Colon Ident("Path") Comma
                    Ident("sync") Colon Ident("Path") BraceClose
                Assign Ident("load_config") ParenOpen String("\\\"\r\t\u{1234}") ParenClose Question Semi

                RawString(r#"\"'\r"#) Semi
                RawString(r"\r") Semi
                RawString("H\"") Semi
                Number(10.4) Semi
                Number(0x10) Semi
                Number(0o1_00) Semi
                Number(0b01_0000) Semi
                Number(0.1_2) Semi
                Number(0.1E10) Semi
                Number(0.1e-10) Semi
            BraceClose
        }
        assert!(tokens.next().is_none());
        // assert_eq!(tokens.next().unwrap(), Token::Ident("pre".into()));
        // assert_eq!(tokens.next().unwrap(), Token::Ident("fn".into()));
    }
}
