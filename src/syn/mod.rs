#![allow(dead_code)]
use std::{fmt::Display, iter};

use crate::tokenizer::{
    ParseBuffer, ParseToken, ParseTokenExt, Span, Token, TokenBuffer, TokenKind,
};

#[macro_use]
mod macros;
#[macro_use]
mod lisp;

#[macro_use]
pub mod token;
use token::*;

use self::{
    expr::Expression,
    lisp::{Lisp, ToLisp},
};

mod expr;

const TAB: &str = "    ";

pub trait Parse: Sized {
    fn parse(tokens: &mut ParseBuffer) -> Result<Self>;
}

pub trait ParseDelimited: Sized {
    fn parse_delimited<'source>(
        tokens: &mut ParseBuffer<'source>,
    ) -> Result<(Self, ParseBuffer<'source>)>;
}

trait ParseStream<'source> {
    fn parse<T: Parse>(&mut self) -> Result<T>;
    fn parse_delimited<T: ParseDelimited>(&mut self) -> Result<(T, TokenBuffer<'source>)>;
    fn parse_seperated<T: Parse, S: Parse>(&mut self) -> Result<Vec<T>>;
    fn parse_terminated<T: Parse, P: Parse>(&mut self) -> Result<T>;
}

impl<'source> ParseBuffer<'source> {
    fn parse<T: Parse>(&mut self) -> Result<T> {
        T::parse(self)
    }
    fn parse_delimited<T: ParseDelimited>(&mut self) -> Result<(T, ParseBuffer<'source>)> {
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
        let inner = *self;
        for i in 0..self.tokens.len() {
            let current = *self;
            if self
                .parse::<Option<P>>()
                .expect("Option always parses")
                .is_some()
            {
                *self = current;
                return T::parse(&mut ParseBuffer {
                    tokens: &inner.tokens[0..=i],
                });
            } else {
                self.next().expect("should never reach end")?;
            }
        }
        let mut inner = inner;
        T::parse(&mut inner)
        // let mut tokens = iter::from_fn(|| {
        //     if self.peek().is_none() {
        //         None
        //     } else {
        //         let current = self.clone();
        //         if self
        //             .parse::<Option<P>>()
        //             .expect("Option always parses")
        //             .is_some()
        //         {
        //             *self = current;
        //             None
        //         } else {
        //             Some(self.next().expect("none is filtered already").clone())
        //         }
        //     }
        // })
        // .collect();
        // T::parse(&mut &mut tokens)
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
#[derive(Debug, Clone)]
pub struct Error {
    span: Span,
    msg: String,
}

impl Error {
    pub fn new(span: Span, msg: String) -> Self {
        Self { span, msg }
    }
    pub fn eof(msg: String) -> Self {
        Self {
            span: Span::EOF,
            msg,
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

impl<T: Parse> Parse for Vec<T> {
    fn parse(tokens: &mut ParseBuffer) -> Result<Self> {
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
    fn parse(tokens: &mut ParseBuffer) -> Result<Self> {
        let last = *tokens;

        if let Ok(t) = tokens.parse() {
            Ok(Some(t))
        } else {
            *tokens = last;
            Ok(None)
        }
    }
}

#[derive(Debug)]
pub struct Script {
    pub fns: Vec<Function>,
}

impl ToLisp for Script {
    fn to_lisp(&self) -> Lisp {
        self.fns.to_lisp()
    }
}

impl Parse for Script {
    fn parse(tokens: &mut ParseBuffer) -> Result<Self> {
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

impl ToLisp for Function {
    fn to_lisp(&self) -> Lisp {
        let Self {
            vis,
            name,
            args,
            body,
            ..
        } = self;
        // let vis = vis.to_lisp();
        // let name = name.map(|name| name.name).unwrap_or_default();
        // let args = args
        //     .iter()
        //     .map(ToLisp::to_lisp)
        //     .collect::<Vec<_>>()
        //     .join(" ");
        // let body = body
        //     .iter()
        //     .map(|stmt| format!("{TAB}{}", stmt.to_lisp()))
        //     .collect::<Vec<_>>()
        //     .join("\n");
        // format!("(fn ({vis} {name}) ({args})\n{body}\n)")
        lisp![("fn" (vis name) args body)]
    }
}

#[derive(Debug)]
pub struct DocComment {
    comment: String,
    span: Span,
}

impl Parse for DocComment {
    fn parse(tokens: &mut ParseBuffer) -> Result<Self> {
        let (mut comment, mut span) = match tokens.next().normal()? {
            Some(Token {
                kind: TokenKind::DocComment(comment),
                span,
            }) => (comment.to_string(), span.to_owned()),
            other => unexpected!(other, expected DocComment),
        };
        while matches!(
            tokens.peek(),
            Some(Token {
                kind: TokenKind::DocComment(_),
                ..
            })
        ) {
            match tokens.next().transpose()? {
                Some(ParseToken::Normal(Token {
                    kind: TokenKind::DocComment(line),
                    span: line_span,
                })) => {
                    comment.push('\n');
                    comment.push_str(line);
                    span.extend(*line_span);
                }
                _ => unreachable!("Only loops when there are comments"),
            }
        }
        Ok(Self { comment, span })
    }
}

impl Parse for Function {
    fn parse(tokens: &mut ParseBuffer) -> Result<Self> {
        let mut args;
        let mut body;
        Ok(Self {
            doc: tokens.parse()?,
            vis: tokens.parse()?,
            fun: tokens.parse()?,
            name: tokens.parse()?,
            paren: delimited!(args in tokens),
            args: (&mut args).parse_seperated::<_, Comma>()?,
            brace: delimited!(body in tokens),
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
    fn parse(tokens: &mut ParseBuffer) -> Result<Self> {
        any! {tokens:
            value => Self::Hid(value),
            value => Self::Pub(value),
            value => Self::Pre(value),
            value => Self::Post(value),
        };
        unexpected!(expected VisKind in tokens);
    }
}

#[derive(Debug)]
pub enum Hook {
    All(Asterix),
    Name(Ident),
}

impl ToLisp for Hook {
    fn to_lisp(&self) -> Lisp {
        match self {
            Hook::All(_) => lisp!("*"),
            Hook::Name(ident) => lisp!(ident),
        }
    }
}

impl Parse for Hook {
    fn parse(tokens: &mut ParseBuffer) -> Result<Self> {
        any! {tokens:
            value => Self::All(value),
            value => Self::Name(value),
        };
        unexpected!(expected VisKind in tokens);
    }
}

#[derive(Debug)]
pub struct Vis {
    kind: Option<VisKind>,
    paren: Option<Paren>,
    hooks: Option<Vec<Hook>>,
    span: Span,
}

impl ToLisp for Vis {
    fn to_lisp(&self) -> Lisp {
        let kind = match self.kind {
            Some(VisKind::Hid(_)) => "hid".to_owned(),
            Some(VisKind::Pub(_)) => "pub".to_owned(),
            Some(VisKind::Pre(_)) => "pre".to_owned(),
            Some(VisKind::Post(_)) => "post".to_owned(),
            None => String::new(),
        };
        let hooks = &self.hooks;
        lisp![(kind hooks)]
    }
}

impl Parse for Vis {
    fn parse(tokens: &mut ParseBuffer) -> Result<Self> {
        let kind = tokens.parse()?;

        let (paren, hooks, span) = if matches!(kind, Some(VisKind::Pre(_) | VisKind::Post(_)))
            && matches!(
                tokens.peek(),
                Some(Token {
                    kind: TokenKind::ParenOpen,
                    ..
                })
            ) {
            let mut inner;
            let paren: Paren = delimited!(inner in tokens);
            let span = paren.span;
            (Some(paren), Some((&mut inner).parse()?), span)
        } else {
            // TODO
            (None, None, Span::EOF)
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

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.name.fmt(f)
    }
}

impl ToLisp for Ident {
    fn to_lisp(&self) -> Lisp {
        self.name.to_lisp()
    }
}

impl Parse for Ident {
    fn parse(tokens: &mut ParseBuffer) -> Result<Self> {
        match tokens.next().normal()? {
            Some(Token {
                kind: TokenKind::Ident(name),
                span,
            }) => Ok(Self {
                name: name.to_string(),
                span: *span,
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

impl ToLisp for Argument {
    fn to_lisp(&self) -> Lisp {
        let Self { name, ty, .. } = self;
        lisp![(":" name ty)]
    }
}

impl Parse for Argument {
    fn parse(tokens: &mut ParseBuffer) -> Result<Self> {
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

impl ToLisp for Statement {
    fn to_lisp(&self) -> Lisp {
        if let Some(Assignment { vis, name, ty, .. }) = &self.assignment {
            // let vis = vis.map(ToString::to_string).unwrap_or_default();
            // format!("(let ({vis} {name} {ty}))");
            lisp![("let" (vis name ty) {self.expr})]
        } else {
            lisp!(self.expr)
        }
    }
}

impl Parse for Statement {
    fn parse(tokens: &mut ParseBuffer) -> Result<Self> {
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
    fn parse(tokens: &mut ParseBuffer) -> Result<Self> {
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
    fn parse(tokens: &mut ParseBuffer) -> Result<Self> {
        Ok((tokens.parse()?, tokens.parse()?))
    }
}

impl ToLisp for (Token![:], Type) {
    fn to_lisp(&self) -> Lisp {
        lisp!(self.1)
    }
}

#[derive(Debug)]
pub enum Type {
    Struct(StructType),
    Selection(SelectionType),
    Path(Path),
}

impl ToLisp for Type {
    fn to_lisp(&self) -> Lisp {
        use Type::*;
        defer!(self; Struct(v), Selection(v), Path(v) => lisp!(v))
    }
}

impl Parse for Type {
    fn parse(tokens: &mut ParseBuffer) -> Result<Self> {
        any! {tokens:
            value => Self::Struct(value),
            value => Self::Selection(value),
            value => Self::Path(value),
        }
        unexpected!(expected Type in tokens);
    }
}

#[derive(Debug)]
pub struct StructType {
    brace: Brace,
    fields: Vec<Argument>,
}

impl ToLisp for StructType {
    fn to_lisp(&self) -> Lisp {
        self.fields.to_lisp()
    }
}

impl Parse for StructType {
    fn parse(tokens: &mut ParseBuffer) -> Result<Self> {
        let mut inner;
        Ok(Self {
            brace: delimited!(inner in tokens),
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

impl ToLisp for SelectionType {
    fn to_lisp(&self) -> Lisp {
        let Self { path, in_, expr } = self;
        lisp![(in_ path expr)]
    }
}

impl Parse for SelectionType {
    fn parse(tokens: &mut ParseBuffer) -> Result<Self> {
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
    fn parse(tokens: &mut ParseBuffer) -> Result<Self> {
        Ok(Self(tokens.parse_seperated::<_, Token![::]>()?))
    }
}

impl ToLisp for Path {
    fn to_lisp(&self) -> Lisp {
        lisp!(self.0)
    }
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::tokenize;

    use super::*;

    #[test]
    fn syn() {
        let tokens = &mut tokenize(include_str!("../../test/syntax.fns"));
        let mut tokens = tokens.to_parse();
        let script: Script = tokens.parse().unwrap();
        println!("{:#?}", script.to_lisp());
    }
}
