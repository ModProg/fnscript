#![allow(dead_code)]
use std::{iter, ops::Range};

use crate::tokenizer::{Token, TokenBuffer, TokenStream};

#[macro_use]
mod macros;

#[macro_use]
pub mod token;
use token::*;

use self::expr::Expression;

mod expr;

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
    fn parse_terminated<T: Parse, P: Parse>(&mut self) -> Result<T>;
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
            if self.peek().is_none() || end {
                None
            } else {
                let t = T::parse(self);
                end = Option::<S>::parse(self)
                    .expect("option always parses")
                    .is_none();
                Some(t)
            }
        })
        .collect()
    }

    fn parse_terminated<T: Parse, P: Parse>(&mut self) -> Result<T> {
        let mut tokens = iter::from_fn(|| {
            if self.peek().is_none() {
                None
            } else {
                let current = self.current();
                if self
                    .parse::<Option<P>>()
                    .expect("Option always parses")
                    .is_some()
                {
                    self.reset_to(current);
                    None
                } else {
                    Some(self.next().expect("none is filtered already").clone())
                }
            }
        })
        .collect();
        T::parse(&mut &mut tokens)
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
    fn new(span: Span, msg: String) -> Self {
        Self { span, msg }
    }
    fn eof(msg: String) -> Self {
        Self {
            span: Span {
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
    pub doc: Option<DocComment>,
    pub vis: Vis,
    pub fun: Token![fn],
    pub name: Option<Ident>,
    pub paren: Paren,
    pub args: Vec<Argument>,
    pub brace: Brace,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub struct DocComment {
    comment: String,
    span: Span,
}

impl Parse for DocComment {
    fn parse(tokens: &mut TokenStream) -> Result<Self> {
        let (mut comment, mut span) = match tokens.next() {
            Some((Token::DocComment(comment), span)) => (comment.to_string(), span.to_owned()),
            other => unexpected!(other, expected DocComment),
        };
        while matches!(tokens.peek(), Some((Token::DocComment(_), _))) {
            match tokens.next() {
                Some((Token::DocComment(line), line_span)) => {
                    comment.push('\n');
                    comment.push_str(line);
                    span.end = line_span.end;
                }
                _ => unreachable!("Only loops when there are comments"),
            }
        }
        Ok(Self { comment, span })
    }
}

impl Parse for Function {
    fn parse(tokens: &mut TokenStream) -> Result<Self> {
        let mut args;
        let mut body;
        Ok(Self {
            doc: tokens.parse()?,
            vis: tokens.parse()?,
            fun: tokens.parse()?,
            name: tokens.parse()?,
            paren: delimited!(args in tokens)?,
            args: (&mut args).parse_seperated::<_, Comma>()?,
            brace: delimited!(body in tokens)?,
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
    All(Asterix),
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
    kind: Option<VisKind>,
    paren: Option<Paren>,
    hooks: Option<Vec<Hook>>,
    span: Span,
}

impl Parse for Vis {
    fn parse(tokens: &mut TokenStream) -> Result<Self> {
        let kind = tokens.parse()?;

        let (paren, hooks, span) = if matches!(kind, Some(VisKind::Pre(_) | VisKind::Post(_)))
            && matches!(tokens.peek(), Some((Token::ParenOpen, _)))
        {
            let mut inner;
            let paren: Paren = delimited!(inner in tokens)?;
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
    colon: Token![:],
    // TODO Type
    ty: Type,
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
pub struct Statement {
    assignment: Option<Assignment>,
    expr: Expression,
}

impl Parse for Statement {
    fn parse(tokens: &mut TokenStream) -> Result<Self> {
        Ok(Self {
            assignment: tokens.parse()?,
            expr: (tokens.parse_terminated::<_, Token![;]>())?,
        })
    }
}

#[derive(Debug)]
pub struct Assignment {
    vis: Option<Pub>,
    let_: Let,
    name: Ident,
    ty: Option<(Token![:], Type)>,
    assign: Eq,
}

impl Parse for Assignment {
    fn parse(tokens: &mut TokenStream) -> Result<Self> {
        Ok(Self {
            vis: tokens.parse()?,
            let_: tokens.parse()?,
            name: tokens.parse()?,
            ty: tokens.parse()?,
            assign: tokens.parse()?,
        })
    }
}

impl Parse for (Token![:], Type) {
    fn parse(tokens: &mut TokenStream) -> Result<Self> {
        Ok((tokens.parse()?, tokens.parse()?))
    }
}

#[derive(Debug)]
pub enum Type {
    Struct(StructType),
    Selection(SelectionType),
    Path(Path),
}

impl Parse for Type {
    fn parse(tokens: &mut TokenStream) -> Result<Self> {
        any! {tokens:
            value => Self::Struct(value),
            value => Self::Selection(value),
            value => Self::Path(value),
        }
        unexpected!(tokens.next(), expected Type);
    }
}

#[derive(Debug)]
pub struct StructType {
    brace: Brace,
    fields: Vec<Argument>,
}

impl Parse for StructType {
    fn parse(tokens: &mut TokenStream) -> Result<Self> {
        let mut inner;
        Ok(Self {
            brace: delimited!(inner in tokens)?,
            fields: (&mut inner).parse_seperated::<_, Token![,]>()?,
        })
    }
}

#[derive(Debug)]
pub struct SelectionType {
    path: Path,
    in_: Token![in],
    expr: Expression,
}

impl Parse for SelectionType {
    fn parse(tokens: &mut TokenStream) -> Result<Self> {
        Ok(Self {
            path: tokens.parse()?,
            in_: tokens.parse()?,
            expr: tokens.parse()?,
        })
    }
}

#[derive(Debug)]
pub struct Path(Vec<Ident>);

impl Parse for Path {
    fn parse(tokens: &mut TokenStream) -> Result<Self> {
        Ok(Self(tokens.parse_seperated::<_, Token![::]>()?))
    }
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::tokenize;

    use super::*;

    #[test]
    fn syn() {
        let mut tokens = &mut tokenize(include_str!("../../test/syntax.fns"));
        let script: Script = tokens.parse().unwrap();
        dbg!(script);
    }
}
