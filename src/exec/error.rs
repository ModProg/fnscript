use thiserror::Error;

use crate::{syn::Ident, tokenizer::Span};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Trying to access a variable not declared in scope `{0}` @ {:?}", .0.span)]
    UndeclaredVariableAccess(Ident),
    #[error("Trying to assign the output of an expression evaluating to void @ {0:?}")]
    AssigningVoid(Span),
}
