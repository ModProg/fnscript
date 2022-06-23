use std::borrow::Cow;

use logos::{FilterResult, Lexer, Logos};
use unicode_normalization::UnicodeNormalization;

#[derive(Logos, Debug, PartialEq)]
pub enum Token<'source> {
    // TODO Only normalize if necessary
    #[regex(r"(\p{XID_START}|_)\p{XID_CONTINUE}*", |l|l.slice().nfc().collect::<Cow<str>>())]
    Ident(Cow<'source, str>),

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
    #[token(":")]
    Colon,
    #[token("=")]
    Eq,
    #[error]
    #[regex(r"\p{Whitespace}", logos::skip)]
    #[regex(r"(//|////)[^\n]*\n", logos::skip)]
    #[regex(r"#![^\n]*\n", |l| if l.span().start == 0 {FilterResult::Skip} else {FilterResult::Error})]
    Error,
}

impl<'source> PartialEq<&str> for Token<'source> {
    fn eq(&self, string: &&str) -> bool {
        match self {
            Self::Ident(ident) => ident == string,
            _ => false,
        }
    }
}

pub fn tokenize(input: &str) -> Lexer<Token> {
    Token::lexer(input)
}

#[cfg(test)]
mod test {
    use crate::tokenizer::{tokenize, Token};

    macro_rules! token_test {
        {$tokens:ident => Ident($ident:tt) $($tts:tt)*} => {
            assert_eq!($tokens.next().unwrap(), Token::Ident($ident.into()));
            token_test!{$tokens => $($tts)*}
        };
        {$tokens:ident => $token:ident $($tts:tt)*} => {
            assert_eq!($tokens.next().unwrap(), Token::$token);
            token_test!{$tokens => $($tts)*}
        };
        {$tokens:ident =>} => {}
    }

    #[test]
    fn test() {
        let mut tokens = tokenize(include_str!("../example.fns"));
        token_test! { tokens =>
            Ident("pre") Ident("fn") ParenOpen ParenClose BraceOpen
                Ident("pub") Ident("let") Ident("config") Colon 
                BraceOpen Ident("source") Colon Ident("Path") BraceClose
                Eq Ident("load_config") ParenOpen
        }
        // assert_eq!(tokens.next().unwrap(), Token::Ident("pre".into()));
        // assert_eq!(tokens.next().unwrap(), Token::Ident("fn".into()));
    }
}

// pub fn tokenize(input: &str) -> IResult<&str, Vec<Token>> {
//     many0(delimited(
//         tuple((space0, opt(comment), space0)),
//         token,
//         space0,
//     ))(input)
// }
//
// fn token(input: &str) -> IResult<&str, Token> {
//     alt((ident,group,))(input)
// }
//
// fn comment(input: &str) -> IResult<&str, ()> {
//     if input.starts_with(pat)
//     let (input, _) = input.split_at_position_complete(|c| c == '\n')?;
//     Ok((input, ()))
// }
//
// fn space(input: &str) {
//
// }
