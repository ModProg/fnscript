#![allow(dead_code)]
use std::{iter, ops::Range};

use crate::tokenizer::{Token, TokenBuffer, TokenStream};

#[macro_use]
pub mod token;
use token::*;

pub trait Parse: Sized {
    fn parse(tokens: &mut TokenStream) -> Result<Self>;
}

pub trait ParseDelimited: Sized {
    fn parse_delimited<'source>(
        tokens: &mut TokenStream<'source>,
    ) -> Result<(Self, TokenBuffer<'source>)>;
}

trait ParseStream<'source> {
    fn parse<T: Parse>(&mut self) -> Result<T>;
    fn parse_delimited<T: ParseDelimited>(&mut self) -> Result<(T, TokenBuffer<'source>)>;
    fn parse_seperated<T: Parse, S: Parse>(&mut self) -> Result<Vec<T>>;
}

impl<'source> ParseStream<'source> for TokenStream<'source> {
    fn parse<T: Parse>(&mut self) -> Result<T> {
        T::parse(self)
    }
    fn parse_delimited<T: ParseDelimited>(&mut self) -> Result<(T, TokenBuffer<'source>)> {
        T::parse_delimited(self)
    }

    fn parse_seperated<T: Parse, S: Parse>(&mut self) -> Result<Vec<T>> {
        let mut end = false;
        iter::from_fn(|| {
            let t = T::parse(self);
            end = Option::<S>::parse(self).expect("option always parses").is_none();
            Some(t)
        })
        .collect()
    }
}

// TODO bette range :D
// #[derive(Debug)]
// pub struct Span {
//     start: Position,
//     end: Position,
// }
//
// #[derive(Debug)]
// pub struct Position {
//     line: usize,
//     column: usize,
// }
type Span = Range<usize>;

#[derive(Debug)]
pub struct Error {
    span: Span,
    msg: String,
}

impl Error {
    fn new(span: std::ops::Range<usize>, msg: String) -> Self {
        Self { span, msg }
    }
    fn eof(msg: String) -> Self {
        Self {
            span: Range {
                start: usize::MAX,
                end: usize::MAX,
            },
            msg,
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

impl<T: Parse> Parse for Vec<T> {
    fn parse(tokens: &mut TokenStream) -> Result<Self> {
        iter::from_fn(|| {
            if tokens.peek().is_some() {
                Some(tokens.parse())
            } else {
                None
            }
        })
        .collect()
    }
}

impl<T: Parse> Parse for Option<T> {
    fn parse(tokens: &mut TokenStream) -> Result<Self> {
        let last = tokens.current();

        if let Ok(t) = tokens.parse() {
            Ok(Some(t))
        } else {
            tokens.reset_to(last);
            Ok(None)
        }
    }
}

#[derive(Debug)]
pub struct Script {
    pub fns: Vec<Function>,
}

impl Parse for Script {
    fn parse(tokens: &mut TokenStream) -> Result<Self> {
        Ok(Self {
            fns: tokens.parse()?,
        })
    }
}

#[derive(Debug)]
pub struct Function {
    pub vis: Vis,
    pub fun: Token!(fn),
    pub name: Option<Ident>,
    pub paren: Paren,
    pub args: Vec<Argument>,
    pub brace: Brace,
    pub body: Vec<Statement>,
}

impl Parse for Function {
    fn parse(tokens: &mut TokenStream) -> Result<Self> {
        let mut args;
        let mut body;
        Ok(Self {
            vis: tokens.parse()?,
            fun: tokens.parse()?,
            name: tokens.parse()?,
            paren: delimited!(tokens, args)?,
            args: (&mut args).parse_seperated::<_, Comma>()?,
            brace: delimited!(tokens, body)?,
            body: (&mut body).parse_seperated::<_, Semi>()?,
        })
    }
}

#[derive(Debug)]
pub enum VisKind {
    Hid(Hid),
    Pub(Pub),
    Pre(Pre),
    Post(Post),
}

macro_rules! any {
    {
        $tokens:ident:
        $($pat:pat => $target:expr),* $(,)?
    } => {
        let current = $tokens.current();
        $(
            if let Ok($pat) = $tokens.parse() {
                return Ok($target);
            } else {
                $tokens.reset_to(current);
            }
        )*

    };
}

impl Parse for VisKind {
    fn parse(tokens: &mut TokenStream) -> Result<Self> {
        any! {tokens:
            value => Self::Hid(value),
            value => Self::Pub(value),
            value => Self::Pre(value),
            value => Self::Post(value),
        };
        unexpected!(tokens.next(), expected VisKind);
    }
}

#[derive(Debug)]
pub enum Hook {
    All(Mul),
    Name(Ident),
}

impl Parse for Hook {
    fn parse(tokens: &mut TokenStream) -> Result<Self> {
        any! {tokens:
            value => Self::All(value),
            value => Self::Name(value),
        };
        unexpected!(tokens.next(), expected VisKind);
    }
}

#[derive(Debug)]
pub struct Vis {
    kind: VisKind,
    paren: Option<Paren>,
    hooks: Option<Vec<Hook>>,
    span: Span,
}

impl Parse for Vis {
    fn parse(tokens: &mut TokenStream) -> Result<Self> {
        let kind = tokens.parse()?;

        let (paren, hooks, span) = if matches!(kind, VisKind::Pre(_) | VisKind::Post(_))
            && matches!(tokens.peek(), Some((Token::ParenOpen, _)))
        {
            let mut inner;
            let paren: Paren = delimited!(tokens, inner)?;
            let span = paren.span.clone();
            (Some(paren), Some((&mut inner).parse()?), span)
        } else {
            // TODO
            (None, None, 0..0)
        };

        Ok(Vis {
            kind,
            paren,
            hooks,
            span,
        })
    }
}

#[derive(Debug)]
pub struct Ident {
    name: String,
    span: Span,
}

impl Parse for Ident {
    fn parse(tokens: &mut TokenStream) -> Result<Self> {
        match tokens.next() {
            Some((Token::Ident(name), span)) => Ok(Self {
                name: name.to_string(),
                span: span.clone(),
            }),
            other => unexpected!(other, expected Ident),
        }
    }
}

#[derive(Debug)]
pub struct Argument {
    name: Ident,
    colon: Token!(:),
    // TODO Type
    ty: Ident,
}

impl Parse for Argument {
    fn parse(tokens: &mut TokenStream) -> Result<Self> {
        Ok(Self {
            name: tokens.parse()?,
            colon: tokens.parse()?,
            ty: tokens.parse()?,
        })
    }
}

#[derive(Debug)]
pub struct Statement {}

impl Parse for Statement {
    fn parse(tokens: &mut TokenStream) -> Result<Self> {
        while !matches!(tokens.peek(), Some((Token::Semi, _))) {
            tokens.next();
        }
        Ok(Self {})
    }
}
