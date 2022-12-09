use std::collections::HashMap;

mod error;
use derive_more::IsVariant;
pub use error::*;

use crate::syn::{
    expr::Expr, Function, Hook, Ident, Path, Script, Spanned, Statement, Type, Vis, VisKind,
};

pub struct Variables(HashMap<String, (Value, Option<Type>)>);
impl Variables {
    fn try_set(&mut self, name: &str, value: Value, ty: Option<Type>) -> Result<()> {
        if let Some((current_value, current_type)) = self.0.get(name) {
            // TODO check if types are compatible
        }
        Ok(())
    }
    fn try_create(&mut self, name: &str, value: Value, ty: Option<Type>) -> Result<()> {

        if let Some((current_value, current_type)) = self.0.get(name) {
            // TODO check if types are compatible
        }
        Ok(())
    }
}

#[derive(IsVariant)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    None,
    Void,
}

impl Script {
    pub fn get_pre_fns<'a>(&'a self, for_fn: &'a str) -> impl Iterator<Item = &'a Function> {
        self.fns.iter().filter(|fun| fun.vis.is_pre(for_fn))
    }
}

impl Function {
    pub fn exec(
        &self,
        globals: &mut Variables,
        args: Variables, /* , positionals: &mut Variables */
    ) -> Result<()> {
        let mut locals = args;
        // TODO refactor this assignments away you LARK
        for stmt @ Statement { assignment, expr } in &self.body {
            let res = expr.exec(globals, &mut locals)?;
            if let Some(assignment) = assignment {
                if res.is_void() {
                    return Err(Error::AssigningVoid(stmt.span()));
                }
                if assignment.is_pub() {
                    // TODO this should be verified in linting
                    assert!(assignment.is_let());
                    globals.try_create()
                }
            }
        }
        Ok(())
    }
}

impl Expr {
    pub fn exec<'a>(
        &'a self,
        globals: &'a mut Variables,
        locals: &'a mut Variables,
    ) -> Result<&'a Value> {
        match self {
            Expr::Variable(path) => {
                let ident = path.expect_ident().as_str();
                locals
                    .get(ident)
                    .or_else(|| globals.get(ident))
                    .ok_or_else(|| Error::UndeclaredVariableAccess(path.expect_ident().clone()))
            }
            Expr::Field(_) => todo!(),
            Expr::Lit(_) => todo!(),
            Expr::Call(_) => todo!(),
            Expr::PreUnary(_) => todo!(),
            Expr::PostUnary(_) => todo!(),
            Expr::Binary(_) => todo!(),
            Expr::If(_) => todo!(),
            Expr::MethodCall(_) => todo!(),
            Expr::Index(_) => todo!(),
            Expr::Struct(_) => todo!(),
            Expr::Tuple(_) => todo!(),
            Expr::Paren(_) => todo!(),
            Expr::Return(_) => todo!(),
            Expr::Array(_) => todo!(),
            Expr::Block(_) => todo!(),
        }
    }
}

impl Path {
    pub fn expect_ident(&self) -> &Ident {
        assert_eq!(self.segments.len(), 1);
        &self.segments[0]
    }
}

impl Ident {
    pub fn as_str(&self) -> &str {
        self.name.as_str()
    }
}

impl Vis {
    pub fn is_pub(&self) -> bool {
        matches!(self.kind, Some(VisKind::Pub(_)))
    }
    pub fn is_pre(&self, for_fn: &str) -> bool {
        matches!(self.kind, Some(VisKind::Pre(_)))
            && if let Some(hooks) = &self.hooks {
                hooks.iter().any(|hook| {
                    hook.is_all() || matches!( hook, Hook::Name(ident) if ident == for_fn)
                })
            } else {
                false
            }
    }
}
