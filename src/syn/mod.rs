#![allow(dead_code)]
use std::{
    cmp,
    fmt::{Debug, Display},
    iter,
};

use derive_more::IsVariant;
pub use fns_macros::Parse;
use getset::{CopyGetters, Getters};

pub use crate::tokenizer::{
    ParseBuffer, ParseToken, ParseTokenExt, Span, Spanned, SpannedOption, Token, TokenKind,
};

#[macro_use]
mod macros;
#[macro_use]
mod lisp;

#[macro_use]
pub mod token;
use token::*;

use self::{
    expr::Expr,
    lisp::{Lisp, ToLisp},
};

pub mod expr;

const TAB: &str = "    ";

pub trait Parse: Sized {
    fn parse(tokens: &mut ParseBuffer) -> Result<Self>;
}

pub trait ParseDelimited: Sized {
    fn parse_delimited<'source>(
        tokens: &mut ParseBuffer<'source>,
    ) -> Result<(Self, ParseBuffer<'source>)>;
}

pub trait Peek {
    fn peek(tokens: &ParseBuffer) -> bool;
}

impl<T: Parse> Peek for T {
    fn peek(tokens: &ParseBuffer) -> bool {
        let mut copy = *tokens;
        T::parse(&mut copy).is_ok()
    }
}

impl<'source> ParseBuffer<'source> {
    fn peek<T: Peek>(&self) -> bool {
        T::peek(self)
    }

    pub fn parse<T: Parse>(&mut self) -> Result<T> {
        T::parse(self)
    }
    fn parse_delimited<T: ParseDelimited>(&mut self) -> Result<(T, ParseBuffer<'source>)> {
        T::parse_delimited(self)
    }

    fn parse_seperated<T: Parse, S: Parse>(&mut self) -> Result<Vec<T>> {
        let mut end = false;
        iter::from_fn(|| {
            if self.is_empty() || end {
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

    fn parse_seperated_complete<T: Parse, S: Parse>(&mut self) -> Result<Vec<T>> {
        iter::from_fn(|| {
            if self.is_empty() {
                None
            } else {
                let t = T::parse(self);
                if !self.is_empty() {
                    if let Err(e) = S::parse(self) {
                        return Some(Err(e));
                    }
                }
                Some(t)
            }
        })
        .collect()
    }

    fn parse_terminated<T: Parse, P: Parse + Debug>(&mut self) -> Result<T> {
        let inner = *self;
        let mut i = 0;
        while let Some(token) = self.next().transpose()? {
            let mut fork = *self;
            // TODO indexing here works not as expected, reason are groups lmao
            // still not even though I thought I fixed it.....asdhjkgajklödghb b
            i += token.len();
            if let Some(_terminator) = fork.parse::<Option<P>>().expect("Option always parses") {
                let inner = &mut ParseBuffer {
                    // FIXME only usable with 1 len terminators
                    tokens: &inner.tokens[0..=i + fork.tokens.len() - self.tokens.len()],
                };

                let ret = inner.parse();
                expect!(empty, inner);
                return ret;
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
#[derive(Debug, Clone, thiserror::Error)]
#[error("{msg}")]
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
            if tokens.is_empty() {
                None
            } else {
                Some(tokens.parse())
            }
        })
        .collect()
    }
}

impl<T: Parse> Parse for Box<T> {
    fn parse(tokens: &mut ParseBuffer) -> Result<Self> {
        Ok(Box::new(tokens.parse()?))
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

#[derive(Debug, Parse, ToLisp, Spanned)]
#[lisp(fns)]
pub struct Script {
    pub doc: Option<ScriptDocComment>,
    pub fns: Vec<Function>,
}

#[derive(Debug, Parse, ToLisp, Spanned)]
#[lisp(("fn" (vis name) args body))]
pub struct Function {
    pub doc: Option<DocComment>,
    pub vis: Vis,
    pub fun: Token![fn],
    pub name: Option<Ident>,
    #[parse(inner = args)]
    pub paren: Paren,
    #[parse(seperator = Token![,], complete)]
    pub args: Vec<Argument>,
    #[parse(inner = body)]
    pub brace: Brace,
    #[parse(seperator = Token![;], complete)]
    pub body: Vec<Statement>,
}

#[derive(Debug, Spanned)]
pub struct ScriptDocComment {
    #[span(ignored)]
    pub comment: String,
    span: Span,
}

impl Parse for ScriptDocComment {
    fn parse(tokens: &mut ParseBuffer) -> Result<Self> {
        let (mut comment, mut span) = match tokens.next().normal()? {
            Some(Token {
                kind: TokenKind::ScriptDocComment(comment),
                span,
            }) => (comment.trim_end().to_string(), span.to_owned()),
            other => unexpected!(other, expected ScriptDocComment),
        };
        while tokens.peek::<ScriptDocComment>() {
            match tokens.next().transpose()? {
                Some(ParseToken::Normal(Token {
                    kind: TokenKind::ScriptDocComment(line),
                    span: line_span,
                })) => {
                    comment.push('\n');
                    comment.push_str(line.trim_end());
                    span.extend(*line_span);
                }
                _ => unreachable!("Only loops when there are comments"),
            }
        }
        Ok(Self { comment, span })
    }
}

#[derive(Debug, Spanned)]
pub struct DocComment {
    #[span(ignored)]
    pub comment: String,
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
        while tokens.peek::<DocComment>() {
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

#[derive(Debug, Parse, ToLisp, Spanned)]
pub enum VisKind {
    Hid(Token![hid]),
    Pub(Token![pub]),
    Pre(Token![pre]),
    Post(Token![post]),
}

#[derive(Debug, Parse, ToLisp, IsVariant, Spanned)]
pub enum Hook {
    All(Token![*]),
    Help(Token![help]),
    Name(Ident),
}

#[derive(Debug, ToLisp, Spanned)]
#[lisp((kind hooks))]
pub struct Vis {
    pub span: Span,
    pub kind: Option<VisKind>,
    pub paren: Option<Paren>,
    pub hooks: Option<Vec<Hook>>,
}

impl Parse for Vis {
    fn parse(tokens: &mut ParseBuffer) -> Result<Self> {
        let kind = tokens.parse()?;

        // TODO verify behaviour for pre(help) (){} and pre (){}
        let (paren, hooks, span) =
            if matches!(kind, Some(VisKind::Pre(_) | VisKind::Post(_))) && tokens.peek::<Paren>() {
                let mut inner;
                let paren: Paren = delimited!(inner in tokens);
                let span = paren.span;
                (
                    Some(paren),
                    Some(inner.parse_seperated_complete::<_, Token![,]>()?),
                    span,
                )
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

#[derive(Debug, ToLisp, Clone, Spanned)]
#[lisp(name)]
pub struct Ident {
    #[span(ignored)]
    pub name: String,
    pub span: Span,
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.name, f)
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

impl cmp::PartialEq<str> for Ident {
    fn eq(&self, other: &str) -> bool {
        self.name == other
    }
}

#[derive(Debug, Parse, ToLisp, Spanned)]
#[lisp((":" name ty))]
pub struct Argument {
    pub name: Ident,
    colon: Token![:],
    pub ty: Type,
}

#[derive(Debug, Spanned)]
pub struct Statement {
    pub assignment: Option<Assignment>,
    pub expr: Expr,
}

impl ToLisp for Statement {
    fn to_lisp(&self) -> Lisp {
        if let Some(Assignment { vis, name, ty, .. }) = &self.assignment {
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
            // TODO allow if, loop, for and while to not have a semicolon when not used as
            // expression
            expr: (tokens.parse_terminated::<_, Token![;]>())?,
        })
    }
}

#[derive(Debug, Parse, Spanned)]
pub struct Assignment {
    vis: Option<Pub>,
    let_: Option<Let>,
    name: Ident,
    ty: Option<(Token![:], Type)>,
    assign: Eq,
}

impl Assignment {
    pub fn is_pub(&self) -> bool {
        self.vis.is_some()
    }
    pub fn is_let(&self) -> bool {
        self.let_.is_some()
    }
    pub fn name(&self) -> &str {
        self.name.as_str()
    }
    pub fn ty(&self) -> Option<&Type> {
        self.ty.map(|t| t.1).as_ref()
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

impl Spanned for (Token![:], Type) {
    fn span(&self) -> Span {
        self.0.span() + self.1.span()
    }
}

#[derive(Debug, Parse, ToLisp, Spanned)]
pub enum Type {
    Struct(StructType),
    Selection(SelectionType),
    Path(Path),
}

#[derive(Debug, Parse, Spanned)]
pub struct StructType {
    #[parse(inner = fields)]
    brace: Brace,
    #[parse(complete, seperator = Token![,])]
    fields: Vec<Argument>,
}

impl ToLisp for StructType {
    fn to_lisp(&self) -> Lisp {
        Lisp::Multiple(
            iter::once(lispm!(struct))
                .chain(self.fields.iter().map(ToLisp::to_lisp))
                .collect(),
        )
    }
}

#[derive(Debug, Parse, ToLisp, Spanned)]
#[lisp((in_ path expr))]
pub struct SelectionType {
    path: Path,
    in_: Token![in],
    expr: Expr,
}

#[derive(Debug, Parse, Spanned)]
pub struct Path {
    #[parse(seperator = Token![::])]
    pub segments: Vec<Ident>,
}

impl ToLisp for Path {
    fn to_lisp(&self) -> Lisp {
        Lisp::Multiple(
            iter::once(lispm!(::))
                .chain(self.segments.iter().map(ToLisp::to_lisp))
                .collect(),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenizer::tokenize;

    #[test]
    fn syn() {
        let tokens = &mut tokenize(include_str!("../../test/syntax.fns"));
        let mut tokens = tokens.to_parse();
        let script: Script = tokens.parse().unwrap();
        let mut fns = script.fns.iter();
        assert_eq!(
            fns.next().unwrap().to_lisp(),
            lispm!(fn (((pre) ()) ()) () (
                (let ((pub) config ((struct (: source (:: Path)) (: sync (:: Path)))))
                    (? ((:: load_config) ("sync_config.toml"))))))
        );
        assert_eq!(
            fns.next().unwrap().to_lisp(),
            lispm!(fn (((pre) ((help))) ()) () (
                (let ((pub) config ((struct (: source (:: Path)) (: sync (:: Path)))))
                    ((:: load_config) ("sync_config.toml")))
                (let ((pub) config_path ())
                    ((:: config_path) ("sync_config.toml")))))
        );
        assert_eq!(
            fns.next().unwrap().to_lisp(),
            lispm!(fn (((pub) ()) (sync)) ((: path (in (:: String) ((:: sys ls) ((. (:: config) source)))))) (
                    // TODO string expansion
                (?((:: move_and_link) ((. (:: config) source) (. (::config) sync) (:: path))))
                "Started syncing of {path} from {config.source + path} at {config.sync + path}"))
        );
        assert_eq!(
            fns.next().unwrap().to_lisp(),
            lispm!(fn (((pub) ()) (load)) ((: path (in (:: String) ((:: sys ls) ((. (:: config) sync)))))) (
                (?((:: move_and_link) ((. (:: config) sync) (. (::config) source) (:: path))))
                "Loaded config of {path} from {config.sync + path} to {config.source + path}"))
        );
        let fun = fns.next().unwrap();
        let mut stmts = fun.body.iter();
        assert_eq!(
            stmts.next().unwrap().to_lisp(),
            lispm!(let (() source ()) (+ (:: from) (:: file)))
        );
        assert_eq!(
            stmts.next().unwrap().to_lisp(),
            lispm!(let (() target ()) (+ (:: to) (:: file)))
        );
        assert_eq!(
            stmts.next().unwrap().to_lisp(),
            lispm!(if (. (:: target) exists ()) (
                    (return (((:: Err) ("Config at {target} already exists"))))
                ) ())
        );
        assert_eq!(
            stmts.next().unwrap().to_lisp(),
            lispm!(if (! (. (:: source) exists ())) (
                    (return (((:: Err) ("Config at {source} does not exist"))))
                )())
        );
        assert_eq!(
            stmts.next().unwrap().to_lisp(),
            lispm!(? ((:: sys mkdir) ("-r" (. (:: target) parent ()))))
        );
        assert_eq!(
            stmts.next().unwrap().to_lisp(),
            lispm!(? ((:: sys mv) ((:: source) (:: target))))
        );
        assert_eq!(
            stmts.next().unwrap().to_lisp(),
            lispm!(? ((:: sys ln) ( "-s" (:: target) (:: source))))
        );
    }

    #[test]
    fn statement() {
        let tokens =
            &mut tokenize("move_and_link(config.source, config.sync, path)?; anotherone(hi);");
        let mut tokens = tokens.to_parse();
        // TODO does not parse ?
        let stmts = tokens
            .parse_seperated_complete::<Statement, Token![;]>()
            .unwrap()
            .to_lisp();

        assert_eq!(
            stmts,
            lispm![
                (? ((:: move_and_link) (
                        (. (:: config) source)
                        (. (:: config) sync)
                        (:: path))))
                ((:: anotherone) (
                        (:: hi)))
            ]
        );
    }
}
