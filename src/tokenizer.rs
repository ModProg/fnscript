use std::{
    borrow::Cow,
    fmt::{Debug, Display},
    iter::{self, Sum},
    ops::Add,
};

use rustc_lexer::unescape::{unescape_raw_str, unescape_str, EscapeError};
use unicode_normalization::UnicodeNormalization;
use unicode_width::UnicodeWidthChar;
use unicode_xid::UnicodeXID;

use crate::syn::{Error, Result};

pub type Lexer<'source> = logos::Lexer<'source, Token<'source>>;
pub type TokenStream<'source> = &'source mut TokenBuffer<'source>;

pub type ParseStream<'a, 'source> = &'a mut ParseBuffer<'source>;

#[derive(Debug, Clone)]
pub struct TokenBuffer<'source> {
    tokens: Vec<Token<'source>>,
}

pub const TAB_WIDTH: usize = 4;

impl<'source> TokenBuffer<'source> {
    pub fn to_parse(&'source self) -> ParseBuffer<'source> {
        ParseBuffer {
            tokens: &self.tokens,
        }
    }

    fn from_source(mut source: &'source str) -> Self {
        let mut column = 1;
        let mut line = 1;
        // Remove Shebang
        if source.starts_with("#!") {
            (_, source) = source.split_once('\n').unwrap_or((source, ""));
            line = 2;
        }
        Self {
            tokens: iter::from_fn(|| {
                let (kind, len) = first_token(source)?;
                let contained = &source[0..len];
                let contained = contained.size();
                // Remove matched token from input
                source = &source[len..];
                Some(Token {
                    span: Span {
                        start: (line, column),
                        end: {
                            if contained.0 > 0 {
                                column = 1;
                            }
                            line += contained.0;
                            column += contained.1;
                            (line, column)
                        },
                    },
                    kind,
                })
            })
            .filter(|token| token.kind != TokenKind::IGNORE)
            .collect(),
        }
    }
}

macro_rules! error {
    ($idx:expr, $($format:tt),*) => {
            return (Error(format!($($format),*)), $idx)
    };
}

macro_rules! match_token {
    ($token:ident in $source:expr, $cond:expr, $end:expr) => {
        if $source.starts_with($cond) {
            return (
                TokenKind::$token,
                $source.find($end).unwrap_or_else(|| $source.len()),
            );
        }
    };
    ($token:ident($_:tt) in $source:expr, $cond:expr, $end:expr, |$var:ident| $map:expr) => {
        match_token!($token($_) in $source, $cond, $end, |$var, end| $map)
    };
    ($token:ident($_:tt) in $source:expr, $cond:expr, $end:expr, |$var:ident, $end_var:ident| $map:expr) => {
        if $source.starts_with($cond) {
            let $end_var = $source.find($end).unwrap_or_else(|| $source.len());
            let $var = &$source[0..$end_var];
            return (TokenKind::$token($map), $end_var);
        }
    };
    ($token:ident in $source:expr, $match:expr) => {
        if $source.starts_with($match) {
            return (TokenKind::$token, $match.len());
        }
    };
}

macro_rules! match_tokens {
    [$source:expr=> $($token:ident($match:expr)),* $(,)?] => {
        $(
            match_token!($token in $source, $match);
        )*
    };
}

macro_rules! match_keyword {
    ($source:expr=> $($token:ident, $cond:literal)+) => {
        $(
            match_keyword!($token in $source, $cond);
        )+
    };
    ($source:expr=> $($token:ident),+ $(,)?) => {
        $(
            match_keyword!($token in $source, &(stringify!($token)).to_ascii_lowercase());
        )+
    };
    ($token:ident in $source:expr, $cond:expr) => {
        if $source.starts_with($cond) && !$source[$cond.len()..].starts_with(char::is_xid_continue) {
            return (TokenKind::$token, $cond.len());
        }
    };
}
macro_rules! unwrap {
    ($cond:expr, $idx:expr, $($format:tt),*) => {
        if let Some(value) = $cond {
            value
        } else {
            error!($idx, $($format),*);
        }
    };
}

macro_rules! unwrap_res {
    ($cond:expr, $idx:expr, $format_lit:literal $(, $($format:expr),*)?) => {
        unwrap_res!($cond, $idx, |_| $format_lit $(, $($format),*)?)
    };
    ($cond:expr, $idx:expr, |$err:ident| $format_lit:literal $(, $($format:expr),*)?) => {
        match $cond {
            Ok(value) => value,
            Err($err) => error!($idx, $format_lit $(, $($format),*)?)
        }
    };
}

// TODO implement way to do gracefull errors
fn first_token(source: &str) -> Option<(TokenKind, usize)> {
    use TokenKind::{Error, Number};

    let mut parse_index = false;
    (!source.is_empty()).then(|| {
        match_token!(IGNORE in source, char::is_whitespace, |c: char| !c.is_whitespace());
        match_token!(ScriptDocComment(_) in source, "//!", '\n', |comment| {
            let comment = comment.trim_start_matches("//!").trim_end();
            comment.strip_prefix(' ').unwrap_or(comment)
        });
        match_token!(IGNORE in source, "////", '\n');
        // Only exactly 3 /// should trigger doc comment
        match_token!(DocComment(_) in source, "///", '\n', |comment| {
            let comment = comment.trim_start_matches("///").trim_end();
            comment.strip_prefix(' ').unwrap_or(comment)
        });
        match_token!(IGNORE in source, "//", '\n');

        // Reset parse_index when anything other than comments is parsed.
        let local_parse_index = parse_index;
        parse_index = false;

        if source.starts_with(|c| c == '"' || c == '\'') {
            let delimiter = source.chars().next().expect("is a quote");
            let mut escaped = false;
            let string_end = if let Some(string_end) = source[1..].find(|c| match c {
                c if delimiter == c && !escaped => true,
                '\\' => {
                    escaped = !escaped;
                    false
                }
                _ => false,
            }) {
                string_end + 1
            } else {
                return (
                    Error(format!(
                        "undelimited string literal, expected `{delimiter}`"
                    )),
                    source.len(),
                );
            };
            let source = &source[1..string_end];
            let mut last_end = 0;
            let mut out = String::new();

            let mut error = None;

            unescape_str(source, &mut |span, res| match res {
                Ok(c) => {
                    out.push_str(&source[last_end..span.start]);
                    out.push(c);
                    last_end = span.end;
                }
                Err(EscapeError::EscapeOnlyChar) if &source[span.start..span.end] == "\"" => {}

                Err(e) => error = Some((Error(format!("Escape Error in string: {e:?}")), span.end)),
            });

            if let Some(error) = error {
                return error;
            }

            out.push_str(&source[last_end..source.len()]);

            return (TokenKind::String(out.into()), string_end + 1);
        }
        if source.starts_with("r#") || source.starts_with("r\"") || source.starts_with("r'") {
            let pounds = source[1..].find(|c| c != '#').unwrap_or_default();
            // FIXME strip_prefix does the wrong thing here
            let delimiter = if source[1 + pounds..].starts_with(|c| c == '"' || c == '\'') {
                source[1 + pounds..]
                    .chars()
                    .next()
                    .expect("string is not empty")
            } else {
                error!(1 + pounds, "Raw string needs to start with `\"` or `'`")
            };
            let end_pattern = delimiter.to_string() + &"#".repeat(pounds);
            let string_end = if let Some(string_end) = source[1 + pounds + 1..].find(&end_pattern) {
                string_end + 1 + pounds + 1
            } else {
                return (
                    Error(format!(
                        "undelimited raw string literal, expected `{end_pattern}`"
                    )),
                    source.len(),
                );
            };
            let source = &source[1 + pounds + 1..string_end];
            let mut last_end = 0;
            let mut out = String::new();

            let mut error = None;

            unescape_raw_str(source, &mut |span, res| match res {
                Ok(c) => {
                    out.push_str(&source[last_end..span.start]);
                    out.push(c);
                    last_end = span.end;
                }
                Err(e) => error = Some((Error(format!("Error in string: {e:?}")), span.end)),
            });

            if let Some(error) = error {
                return error;
            }

            out.push_str(&source[last_end..source.len()]);

            return (TokenKind::RawString(out.into()), string_end + pounds + 1);
        }

        if local_parse_index {
            match_token!(Index(_) in source, char::is_numeric, |c:char| !c.is_numeric(),
                |index, end| match index.parse() {
                    Ok(index) => index,
                    Err(error) => return ( Error(format!("Error parsing index: {error}")), end),
                });
        }

        if source.starts_with('0') && source[1..].starts_with(|c: char| c.is_ascii_alphabetic()) {
            let base = match source[1..]
                .chars()
                .next()
                .expect("first char exists")
                .to_ascii_lowercase()
            {
                'o' => 8,
                'x' => 16,
                'b' => 2,
                c => return (Error(format!("Not a valid number base `{c}`")), 2),
            };
            let number_len = 2 + unwrap!(
                source[2..].find(|c: char| !(c == '_' || c.is_ascii_alphanumeric())),
                source.len(),
                "Number needs digits"
            );
            if number_len == 2 {
                error!(number_len, "Number needs digits");
            }
            let number = unwrap_res!(
                i64::from_str_radix(&source[2..number_len].replace('_', ""), base),
                number_len,
                |e| "Invalid number {e}"
            );
            return (Number(number as f64), number_len);
        }

        if source.starts_with(|c: char| c.is_ascii_digit()) {
            let number_len = source
                .find(|c: char| !(c.is_ascii_digit() || c == '_'))
                .unwrap_or(source.len());
            let number_len = if source[number_len..].starts_with('.') {
                if source[number_len + 1..].starts_with(|c: char| !c.is_ascii_digit()) {
                    number_len
                } else {
                    let number_len = number_len + 1;
                    number_len
                        + source[number_len..]
                            .find(|c: char| !(c.is_ascii_digit() || c == '_'))
                            .unwrap_or(source.len())
                }
                // number_len = number_len + 1 + source[number_len + 1..];
            } else {
                number_len
            };

            let number_len = if source[number_len..].starts_with(|c| c == 'e' || c == 'E') {
                let number_len = number_len + 1;
                let number_len = if source[number_len..].starts_with(|c| c == '+' || c == '-') {
                    number_len + 1
                } else {
                    number_len
                };
                number_len
                    + source[number_len..]
                        .find(|c: char| !(c.is_ascii_digit() || c == '_'))
                        .unwrap_or(source.len())
            } else {
                number_len
            };

            let number = unwrap_res!(
                source[0..number_len].replace('_', "").parse(),
                number_len,
                |e| "Invalid number {e}"
            );

            return (Number(number), number_len);
        }
        // Idents after literals and Keywords to support more things later on
        match_keyword![source=>
            Fn, Let, In, Hid, Pub, Pre, Help, Post, True, False, If, Else, Loop, While, For, Return, Break, Continue,
        ];
        match_token!(Ident(_) in source, char::is_xid_start, |c:char| !c.is_xid_continue(),
            |ident| ident.nfc().collect::<Cow<str>>());

        match_tokens![source=>
            // brackets
            BraceOpen("{"),
            BraceClose("}"),
            ParenOpen("("),
            ParenClose(")"),
            BracketOpen("["),
            BracketClose("]"),
            // punctuation
            Semi(";"),
            Comma(","),
            InklRangeSep("..="),
            RangeSep(".."),
            Question("?"),
            PathSep("::"),
            Colon(":"),
            EqEq("=="),
            Eq("="),
            NotEq("!="),
            Bang("!"),
            LtEq("<="),
            Lt("<"),
            GtEq(">="),
            Gt(">"),
            Arrow("->"),
            Minus("-"),
            AndAssign("&&="),
            And("&&"),
            OrAssign("||="),
            Or("||"),
            PlusAssign("+="),
            Plus("+"),
            MulAssign("*="),
            Asterix("*"),
            DivAssign("/="),
            Div("/"),
            ExpAssign("^="),
            Carret("^"),
            ModAssign("%="),
            Percent("%"),
            // pipes
            Pipe("|>"),
            Pipe("|o>"),
            PipeError("|e>"),
            PipeAll("|eo>"),
            PipeAll("|oe>"),
        ];

        if source.starts_with('.') {
            parse_index = true;
        }

        match_token!(Dot in source, ".");
        todo!("{source}")
    })
}

trait StringExt {
    fn size(&self) -> (usize, usize);
}

impl StringExt for str {
    fn size(&self) -> (usize, usize) {
        let mut width = 0;
        let mut height = 0;
        for c in self.chars() {
            match c {
                '\n' => {
                    width = 0;
                    height += 1;
                }
                '\r' => (),
                '\t' => width += TAB_WIDTH,
                c => width += c.width().unwrap_or_default(),
            };
        }
        (height, width)
    }
}

#[derive(Clone, Copy)]
pub struct ParseBuffer<'source> {
    pub tokens: &'source [Token<'source>],
}

impl Debug for ParseBuffer<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ParseBuffer")
            .field(
                "tokens",
                &self.tokens.iter().map(|t| &t.kind).collect::<Vec<_>>(),
            )
            .finish()
    }
}

impl<'source> ParseBuffer<'source> {
    pub fn fork(&self) -> Self {
        *self
    }
    pub fn is_empty(&self) -> bool {
        self.tokens.is_empty()
    }
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Option<Result<ParseToken<'source>>> {
        let token = self.tokens.first();
        if let Some(token) = token {
            self.tokens = &self.tokens[1..];
            if let Some(delimiter) = &token.kind.delimiter() {
                if let Some(other) = self.next_delimiter(&token.kind, delimiter) {
                    let ret = Some(Ok(ParseToken::Group {
                        start: token,
                        end: &self.tokens[other],
                        inner: ParseBuffer {
                            tokens: &self.tokens[..other],
                        },
                    }));
                    self.tokens = &self.tokens[other + 1..];
                    return ret;
                } else {
                    return Some(Err(Error::new(
                        if let Some(last) = self.tokens.last() {
                            token.span + last.span
                        } else {
                            token.span
                        },
                        format!(
                            "Did not find matching delimiter for `{token}` expected `{delimiter}`"
                        ),
                    )));
                }
            }
        }
        token.map(ParseToken::from).map(Ok)
    }

    fn next_delimiter(&self, left: &TokenKind, right: &TokenKind) -> Option<usize> {
        let mut open = 0;
        self.tokens.iter().position(|token| {
            match &token {
                Token { kind, .. } if kind == left => open += 1,
                Token { kind, .. } if kind == right && open == 0 => return true,
                Token { kind, .. } if kind == right => open -= 1,
                _ => (),
            }
            false
        })
    }

    pub fn next_back(&mut self) -> Option<&'source Token<'source>> {
        let token = self.tokens.last();
        if token.is_some() {
            self.tokens = &self.tokens[..self.tokens.len() - 1];
        }
        token
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Span {
    start: (usize, usize),
    end: (usize, usize),
}
impl Span {
    pub const EOF: Self = Self {
        start: (usize::MIN, usize::MIN),
        end: (usize::MAX, usize::MAX),
    };
    pub const EMPTY: Self = Self {
        start: (usize::MAX, usize::MAX),
        end: (usize::MIN, usize::MIN),
    };
    pub fn extend(&mut self, other: Span) {
        self.start = (
            self.start.0.min(other.start.0),
            self.start.1.min(other.start.1),
        );
        self.end = (self.end.0.max(other.end.0), self.end.1.max(other.end.1));
    }
}

impl Add for Span {
    type Output = Self;

    fn add(mut self, rhs: Self) -> Self::Output {
        self.extend(rhs);
        self
    }
}
impl Add<Option<Span>> for Span {
    type Output = Self;

    fn add(mut self, rhs: Option<Span>) -> Self::Output {
        if let Some(rhs) = rhs {
            self.extend(rhs);
        }
        self
    }
}
impl Add<Span> for Option<Span> {
    type Output = Span;

    fn add(self, rhs: Span) -> Self::Output {
        rhs + self
    }
}

impl Sum for Span {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(Span::EMPTY, Add::add)
    }
}

pub use fns_macros::Spanned;
pub trait Spanned {
    fn span(&self) -> Span;
}

impl<T: Spanned> Spanned for Box<T> {
    fn span(&self) -> Span {
        // We call it on the inner of Box so to say
        (**self).span()
    }
}

impl<T: Spanned> Spanned for Vec<T> {
    fn span(&self) -> Span {
        self.iter().map(Spanned::span).sum()
    }
}

impl Spanned for Span {
    fn span(&self) -> Span {
        *self
    }
}
pub trait SpannedOption {
    fn span(&self) -> Option<Span>;
}

impl<T: Spanned> SpannedOption for Option<T> {
    fn span(&self) -> Option<Span> {
        self.as_ref().map(|s| s.span())
    }
}

#[derive(Debug, Clone)]
pub struct Token<'source> {
    pub span: Span,
    pub kind: TokenKind<'source>,
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for TokenKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(Debug)]
pub enum ParseToken<'source> {
    Normal(&'source Token<'source>),
    Group {
        start: &'source Token<'source>,
        end: &'source Token<'source>,
        inner: ParseBuffer<'source>,
    },
}

impl ParseToken<'_> {
    pub fn span(&self) -> Span {
        match self {
            ParseToken::Normal(token) => token.span,
            ParseToken::Group { start, end, .. } => start.span + end.span,
        }
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        match self {
            ParseToken::Normal(_) => 1,
            ParseToken::Group { inner, .. } => inner.tokens.len() + 2,
        }
    }
}

impl<'source> From<&'source Token<'source>> for ParseToken<'source> {
    fn from(token: &'source Token) -> Self {
        Self::Normal(token)
    }
}

pub trait ParseTokenExt {
    fn normal(&self) -> Result<Option<&Token>>;
}

impl ParseTokenExt for Option<Result<ParseToken<'_>>> {
    fn normal(&self) -> Result<Option<&Token>> {
        Ok(
            match match self {
                Some(Ok(s)) => Some(s),
                Some(Err(e)) => return Err(e.clone()),
                None => None,
            } {
                Some(ParseToken::Normal(token)) => Some(token),
                Some(ParseToken::Group { start, end, .. }) => {
                    return Err(Error::new(
                        start.span + end.span,
                        "Expected simple token found group".to_owned(),
                    ));
                }
                None => None,
            },
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind<'source> {
    // TODO Only normalize if necessary
    Ident(Cow<'source, str>),
    Index(u64),

    // Keywords
    Fn,
    Let,
    In,
    Hid,
    Pub,
    Pre,
    Help,
    Post,
    True,
    False,
    If,
    While,
    Loop,
    Else,
    For,
    Return,
    Break,
    Continue,

    // Literals
    String(Cow<'source, str>),

    RawString(Cow<'source, str>),

    Number(f64),

    // brackets
    BraceOpen,
    BraceClose,
    ParenOpen,
    ParenClose,
    BracketOpen,
    BracketClose,

    // Symbols
    Semi,
    Comma,
    Dot,
    RangeSep,
    InklRangeSep,
    Question,
    Colon,
    PathSep,
    Eq,
    EqEq,
    NotEq,
    Bang,
    Lt,
    LtEq,
    Gt,
    GtEq,
    Arrow,
    Minus,
    And,
    AndAssign,
    Or,
    OrAssign,
    Plus,
    PlusAssign,
    Asterix,
    MulAssign,
    Div,
    DivAssign,
    Carret,
    ExpAssign,
    Percent,
    ModAssign,

    Pipe,
    PipeError,
    PipeAll,

    ScriptDocComment(&'source str),
    DocComment(&'source str),

    // Ignored and errors
    Error(String),

    EOF,
    IGNORE,
}

impl TokenKind<'_> {
    fn delimiter(&self) -> Option<Self> {
        use TokenKind::*;
        match self {
            BraceOpen => Some(BraceClose),
            ParenOpen => Some(ParenClose),
            BracketOpen => Some(BracketClose),
            _ => None,
        }
    }
}

impl<'source> PartialEq<&str> for TokenKind<'source> {
    fn eq(&self, string: &&str) -> bool {
        match self {
            Self::Ident(ident) => ident == string,
            _ => false,
        }
    }
}

pub fn tokenize(input: &str) -> TokenBuffer {
    TokenBuffer::from_source(input)
}

#[cfg(test)]
mod test {
    use crate::tokenizer::{tokenize, TokenKind};

    macro_rules! token_test {
        // {$tokens:ident => $token:ident($ident:tt) $($tts:tt)*} => {
        //     assert_eq!($tokens.next().unwrap(), Token::$token($ident.into()));
        //     token_test!{$tokens => $($tts)*}
        // };
        {$tokens:ident => $token:ident($($inner:tt),*) $($tts:tt)*} => {
            let token = $tokens.next().unwrap();
            assert_eq!(token.kind, TokenKind::$token($($inner.into()),*),"{:?}", token.span);
            token_test!{$tokens => $($tts)*}
        };
        {$tokens:ident => $token:ident $($tts:tt)*} => {
            let token = $tokens.next().unwrap();
            assert_eq!(token.kind, TokenKind::$token,"{:?}", token.span);
            token_test!{$tokens => $($tts)*}
        };
        {$tokens:ident =>} => {}
    }

    #[test]
    fn test() {
        let mut tokens = tokenize(include_str!("../test/tokenize.fns"))
            .tokens
            .into_iter();
        token_test! { tokens =>
            DocComment("Doc comment")
            DocComment("  - This should be indented")
            Pre Fn ParenOpen ParenClose BraceOpen
                Pub Let Ident("config") Colon BraceOpen
                    Ident("source") Colon Ident("Path") Comma
                    Ident("sync") Colon Ident("Path") BraceClose
                Eq Ident("load_config") ParenOpen String("\\\"\r\t\u{1234}") ParenClose Question Semi

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
