use super::*;
// (Range)

#[derive(Debug, ToLisp)]
pub enum Expr {
    Variable(Path),
    Field(ExprField),
    Lit(ExprLit),
    Call(ExprCall),
    PreUnary(ExprPreUnary),
    PostUnary(ExprPostUnary),
    Binary(ExprBinary),
    If(ExprIf),
    MethodCall(ExprMethodCall),
    Index(ExprIndex),
    Struct(ExprStruct),
    Tuple(ExprTuple),
    Paren(ExprParen),
    Return(ExprReturn),
    Array(ExprArray),
    Block(ExprBlock),
}

#[derive(Debug, ToLisp, Parse)]
#[lisp((";" exprs))]
pub struct ExprBlock {
    #[parse(inner = exprs)]
    brace: Token![{}],
    #[parse(seperator = Token![;], complete)]
    exprs: Vec<Statement>,
}

#[derive(Debug, ToLisp, Parse)]
#[lisp((return_ expr))]
pub struct ExprReturn {
    return_: Token![return],
    expr: Option<Box<Expr>>,
}

#[derive(Debug, ToLisp, Parse)]
#[lisp((expr))]
pub struct ExprParen {
    #[parse(inner = expr)]
    paren: Token![()],
    expr: Box<Expr>,
}

#[derive(Debug, ToLisp)]
#[lisp(("[]" expr index))]
pub struct ExprIndex {
    expr: Box<Expr>,
    bracket: Token![[]],
    index: Box<Expr>,
}

#[derive(Debug, ToLisp, Parse)]
#[lisp(("[]" elems))]
pub struct ExprArray {
    #[parse(inner = elems)]
    bracket: Token![[]],
    #[parse(seperator = Token![,], complete)]
    elems: Vec<Expr>,
}

#[derive(Debug, ToLisp, Parse)]
#[lisp(("," fields))]
pub struct ExprTuple {
    #[parse(inner = fields)]
    paren: Token![()],
    #[parse(seperator = Token![,], complete)]
    fields: Vec<Expr>,
}

// TODO support shorthand for a: a
#[derive(Debug, ToLisp, Parse)]
#[lisp(("{}" fields))]
pub struct ExprStruct {
    #[parse(inner = fields)]
    brace: Token![{}],
    #[parse(seperator = Token![,], complete)]
    fields: Vec<FieldValue>,
}

#[derive(Debug, ToLisp, Parse)]
#[lisp((colon member value))]
pub struct FieldValue {
    member: Ident,
    colon: Token![:],
    value: Box<Expr>,
}

#[derive(Debug, ToLisp)]
#[lisp((dot receiver method args))]
pub struct ExprMethodCall {
    receiver: Box<Expr>,
    dot: Token![.],
    method: Ident,
    paren: Paren,
    args: Vec<Expr>,
}

#[derive(Debug, Parse, ToLisp)]
#[lisp((dot base member))]
pub struct ExprField {
    base: Box<Expr>,
    dot: Token![.],
    member: Member,
}

#[derive(Debug, Parse, ToLisp)]
pub enum ExprLit {
    String(LitStr),
    Number(LitNumber),
    Bool(LitBool),
}

#[derive(Debug, ToLisp)]
#[lisp(format!("{value}"))]
pub struct LitBool {
    value: bool,
    span: Span,
}

impl Parse for LitBool {
    fn parse(tokens: &mut ParseBuffer) -> Result<Self> {
        Ok(if tokens.peek::<Token![true]>() {
            let token: Token![true] = tokens.parse()?;
            Self {
                value: true,
                span: token.span,
            }
        } else {
            let token: Token![false] = tokens.parse()?;
            Self {
                value: false,
                span: token.span,
            }
        })
    }
}

// TODO string expansion + raw strings
#[derive(Debug, ToLisp)]
#[lisp(format!("{:?}", value))]
pub struct LitStr {
    value: String,
    span: Span,
}

impl Parse for LitStr {
    fn parse(tokens: &mut ParseBuffer) -> Result<Self> {
        expect! { TokenKind::String(value), span in tokens =>
                Ok(Self{value: value.to_string(), span: *span})
        }
    }
}

#[derive(Debug, ToLisp)]
#[lisp(value.to_string())]
pub struct LitNumber {
    value: f64,
    span: Span,
}

impl Parse for LitNumber {
    fn parse(tokens: &mut ParseBuffer) -> Result<Self> {
        expect! { TokenKind::Number(value), span in tokens =>
                Ok(Self{value: *value, span: *span})
        }
    }
}

#[derive(Debug, ToLisp)]
#[lisp((func args))]
pub struct ExprCall {
    func: Box<Expr>,
    paren: Paren,
    args: Vec<Expr>,
}

#[derive(Debug, ToLisp)]
#[lisp((op expr))]
pub struct ExprPreUnary {
    op: PreUnaryOp,
    expr: Box<Expr>,
}

#[derive(Debug, Parse, ToLisp)]
pub enum PreUnaryOp {
    Bang(Token![!]),
    Neg(Token![-]),
}

#[derive(Debug, ToLisp)]
#[lisp((op expr))]
pub struct ExprPostUnary {
    expr: Box<Expr>,
    op: PostUnaryOp,
}

#[derive(Debug)]
pub struct FnCall {
    span: Span,
}

#[derive(Debug, Parse, ToLisp)]
pub enum PostUnaryOp {
    Question(Question),
    Bang(Bang),
}

#[derive(Debug, ToLisp)]
#[lisp((op lhs rhs))]
pub struct ExprBinary {
    lhs: Box<Expr>,
    op: BinOp,
    rhs: Box<Expr>,
}

// impl Binary {
//     fn parse(tokens: &mut ParseBuffer, precedence: Precedence) -> Result<Expr> {
//         // println!("Parsing {precedence:?} in {tokens:?}");
//         let ops: Vec<_> = iter::once(Ok((None, Expr::parse(tokens, precedence.next())?)))
//             .chain(iter::from_fn(|| {
//                 tokens.peek().is_some().then(|| {
//                     let op = BinOp::parse(tokens, precedence)?;
//                     let expr = Expr::parse(tokens, precedence.next())?;
//                     Ok((Some(op), expr))
//                 })
//             }))
//             .collect::<Result<_>>()?;
//         let mut ops = ops.into_iter().rev();
//         let (op, rhs) = ops.next().expect("There is one entry");
//         if let Some(op) = op {
//             Ok(*ops
//                 .fold((Some(op), Box::new(rhs)), |(op, rhs), (next_op, lhs)| {
//                     let op = op.expect("there should always be an operator");
//                     (
//                         next_op,
//                         Box::new(Expr::Binary(Self {
//                             lhs: Box::new(lhs),
//                             op,
//                             rhs,
//                         })),
//                     )
//                 })
//                 .1)
//         } else {
//             fail!(internal, "only one expression in binary")
//         }
//     }
// }

#[derive(Debug, ToLisp, Parse)]
pub enum BinOp {
    Add(Token![+]),
    Sub(Token![-]),
    Mul(Token![*]),
    Div(Token![/]),
    Rem(Token![%]),
    Exp(Token![^]),
    And(Token![&&]),
    Or(Token![||]),
    Eq(Token![==]),
    Lt(Token![<]),
    Le(Token![<=]),
    Ne(Token![!=]),
    Ge(Token![>=]),
    Gt(Token![>]),
}

// impl BinOp {
//     fn parse(tokens: &mut ParseBuffer, precedence: Precedence) -> Result<Self> {
//         use BinOp::*;
//         Ok(match precedence {
//             Precedence::Or => Or(tokens.parse()?),
//             Precedence::And => And(tokens.parse()?),
//             Precedence::Compare => {
//                 any! {tokens:
//                     Eq,NotEq,Lt,LtEq,Gt,GtEq
//                 }
//                 unexpected!(expected Comparison in tokens)
//             }
//             Precedence::Arithmetic => {
//                 any! {tokens:
//                     Plus, Minus
//                 }
//                 unexpected!(expected "`+` or `-`" in tokens)
//             }
//             Precedence::Term => {
//                 any! {tokens:
//                     Mul, Div
//                 }
//                 unexpected!(expected "`*` or `/`" in tokens)
//             }
//             _ => fail!(internal, "no binary expression"),
//         })
//     }
// }

#[derive(Debug, ToLisp)]
#[lisp((if_ condition body else_))]
pub struct ExprIf {
    if_: Token![if],
    condition: Box<Expr>,
    // #[parse(inner = body)]
    brace: Token![{}],
    // #[parse(seperator = Token![;], complete)]
    body: Vec<Statement>,
    else_: Option<ElseExpr>,
}
impl crate::syn::Parse for ExprIf {
    fn parse(__fns_macros_input: &mut crate::syn::ParseBuffer) -> crate::syn::Result<Self> {
        let mut body;
        ::core::prelude::v1::Ok(Self {
            if_: __fns_macros_input.parse()?,
            condition: __fns_macros_input.parse()?,
            brace: delimited!(body in __fns_macros_input),
            body: (&mut body).parse_seperated_complete::<_, Token![;
            ]>()?,
            else_: __fns_macros_input.parse()?,
        })
    }
}

#[derive(Debug, ToLisp)]
#[lisp((else_ expr))]
pub struct ElseExpr {
    else_: Token![else],
    expr: Box<Expr>,
}

impl Parse for ElseExpr {
    fn parse(tokens: &mut ParseBuffer) -> Result<Self> {
        let else_ = tokens.parse()?;
        let expr = Box::new(if tokens.peek::<Token![if]>() {
            Expr::If(tokens.parse()?)
        } else {
            Expr::Block(tokens.parse()?)
        });

        Ok(Self { else_, expr })
    }
}

#[derive(Debug, ToLisp, Parse)]
#[lisp((else_ (body)))]
pub struct Else {
    else_: Token![else],
    #[parse(inner = body)]
    brace: Token![{}],
    #[parse(seperator = Token![;], complete)]
    body: Vec<Statement>,
}

#[derive(Debug, ToLisp, Parse)]
#[lisp((else_ if_))]
pub struct ElseIf {
    else_: Token![else],
    if_: Box<ExprIf>,
}

#[derive(Debug, ToLisp, Parse)]
pub enum Member {
    Named(Ident),
    Unnamed(Index),
}

#[derive(Debug, ToLisp)]
#[lisp(index)]
pub struct Index {
    index: u64,
    span: Span,
}

impl Parse for Index {
    fn parse(tokens: &mut ParseBuffer) -> Result<Self> {
        match tokens.next().normal()? {
            Some(&Token {
                kind: TokenKind::Index(index),
                span,
            }) => Ok(Self { index, span }),
            other => unexpected!(other, expected Ident),
        }
    }
}

mod parse {
    use crate::tokenizer::ParseStream;

    use super::*;

    impl Parse for Expr {
        fn parse(tokens: &mut ParseBuffer) -> Result<Self> {
            ambiguous_expr(tokens)
        }
    }

    fn ambiguous_expr(input: &mut ParseBuffer) -> Result<Expr> {
        let lhs = unary_expr(input)?;
        parse_expr(input, lhs, Precedence::Any)
    }

    fn unary_expr(input: ParseStream) -> Result<Expr> {
        let begin = input.fork();
        if input.peek::<Token![!]>() || input.peek::<Token![-]>() {
            expr_unary(input).map(Expr::PreUnary)
        } else {
            trailer_expr(begin, input)
        }
    }

    fn expr_unary(input: ParseStream) -> Result<ExprPreUnary> {
        Ok(ExprPreUnary {
            op: input.parse()?,
            expr: Box::new(unary_expr(input)?),
        })
    }

    fn trailer_expr(begin: ParseBuffer, input: ParseStream) -> Result<Expr> {
        let atom = atom_expr(input)?;
        let mut e = trailer_helper(input, atom)?;

        // if let Expr::Verbatim(tokens) = &mut e {
        //     *tokens = verbatim::between(begin, input);
        // } else {
        //     let inner_attrs = e.replace_attrs(Vec::new());
        //     attrs.extend(inner_attrs);
        //     e.replace_attrs(attrs);
        // }

        Ok(e)
    }

    fn trailer_helper(input: ParseStream, mut e: Expr) -> Result<Expr> {
        loop {
            if input.peek::<Paren>() {
                let mut content;
                e = Expr::Call(ExprCall {
                    func: Box::new(e),
                    paren: delimited!(content in input),
                    args: content.parse_seperated_complete::<_, Token![,]>()?,
                });
            } else if input.peek::<Token![.]>()
            // && !input.peek::<Token![..]>()
            // && match e {
            //     Expr::Range(_) => false,
            //     _ => true,
            // }
            {
                let mut dot: Token![.] = input.parse()?;

                let member: Member = input.parse()?;
                // TODO maybe implement Turbofish
                // let turbofish = if member.is_named() && input.peek(Token![::]) {
                //     Some(input.parse::<MethodTurbofish>()?)
                // } else {
                //     None
                // };

                if
                /* turbofish.is_some() || */
                input.peek::<Token![()]>() {
                    if let Member::Named(method) = member {
                        let mut content;
                        e = Expr::MethodCall(ExprMethodCall {
                            receiver: Box::new(e),
                            dot,
                            method,
                            paren: delimited!(content in input),
                            args: content.parse_seperated_complete::<_, Token![,]>()?,
                        });
                        continue;
                    }
                }

                e = Expr::Field(ExprField {
                    base: Box::new(e),
                    dot,
                    member,
                });
            } else if input.peek::<Token!([])>() {
                let mut content;
                e = Expr::Index(ExprIndex {
                    expr: Box::new(e),
                    bracket: delimited!(content in input),
                    index: content.parse()?,
                });
            } else if input.peek::<Token![?]>() || input.peek::<Token![!]>() {
                e = Expr::PostUnary(ExprPostUnary {
                    expr: Box::new(e),
                    op: input.parse()?,
                });
            } else {
                break;
            }
        }
        Ok(e)
    }

    fn block_or_struct(input: ParseStream) -> Result<Expr> {
        let mut content;
        let brace = delimited!(content in input);
        Ok(if content.is_empty() {
            Expr::Struct(ExprStruct {
                brace,
                fields: Vec::new(),
            })
        } else if content.peek::<Ident>()
            && (matches!(content.tokens[1].kind, TokenKind::Colon | TokenKind::Comma))
        {
            Expr::Struct(ExprStruct {
                brace,
                fields: content.parse_seperated_complete::<_, Token![,]>()?,
            })
        } else {
            Expr::Block(ExprBlock {
                brace,
                exprs: content.parse_seperated_complete::<_, Token![;]>()?,
            })
        })
    }

    fn paren_or_tuple(input: ParseStream) -> Result<Expr> {
        let mut content;
        let paren = delimited!(content in input);
        if content.is_empty() {
            return Ok(Expr::Tuple(ExprTuple {
                paren,
                fields: Vec::new(),
            }));
        }

        let first: Expr = content.parse()?;
        if content.is_empty() {
            return Ok(Expr::Paren(ExprParen {
                paren,
                expr: Box::new(first),
            }));
        }
        let _: Token![,] = content.parse()?;
        let mut fields = content.parse_seperated_complete::<_, Token![,]>()?;
        fields.insert(0, first);
        Ok(Expr::Tuple(ExprTuple { paren, fields }))
    }

    fn atom_expr(input: ParseStream) -> Result<Expr> {
        if input.peek::<ExprLit>() {
            input.parse().map(Expr::Lit)
        } else if input.peek::<Ident>() || input.peek::<Token![::]>() {
            input.parse().map(Expr::Variable)
        } else if input.peek::<Token![{}]>() {
            block_or_struct(input)
            // input.parse().map(Expr::Struct)
        } else if input.peek::<Paren>() {
            paren_or_tuple(input)
        } else if input.peek::<Token![return]>() {
            input.parse().map(Expr::Return)
        } else if input.peek::<Bracket>() {
            input.parse().map(Expr::Array)
        // } else if input.peek::<Token![let]>() {
        //     input.parse().map(Expr::Let)
        } else if input.peek::<Token![if]>() {
            dbg!(input).parse().map(Expr::If)
        // } else if input.peek(Token![while]) {
        //     input.parse().map(Expr::While)
        // } else if input.peek(Token![for]) {
        //     input.parse().map(Expr::ForLoop)
        // } else if input.peek(Token![loop]) {
        //     input.parse().map(Expr::Loop)
        // } else if input.peek(Token![match]) {
        //     input.parse().map(Expr::Match)
        // } else if input.peek::<Token![..]> {
        //     expr_range(input).map(Expr::Range)
        // } else if input.peek(Token![_]) {
        //     Ok(Expr::Verbatim(TokenStream::from(
        //         input.parse::<TokenTree>()?,
        //     )))
        // } else if input.peek(Lifetime) {
        //     let the_label: Label = input.parse()?;
        //     let mut expr = if input.peek(Token![while]) {
        //         Expr::While(input.parse()?)
        //     } else if input.peek(Token![for]) {
        //         Expr::ForLoop(input.parse()?)
        //     } else if input.peek(Token![loop]) {
        //         Expr::Loop(input.parse()?)
        //     } else if input.peek(token::Brace) {
        //         Expr::Block(input.parse()?)
        //     } else {
        //         return Err(input.error("expected loop or block expression"));
        //     };
        //     match &mut expr {
        //         Expr::While(ExprWhile { label, .. })
        //         | Expr::ForLoop(ExprForLoop { label, .. })
        //         | Expr::Loop(ExprLoop { label, .. })
        //         | Expr::Block(ExprBlock { label, .. }) => *label = Some(the_label),
        //         _ => unreachable!(),
        //     }
        //     Ok(expr)
        } else {
            unexpected!(expected expression in input);
        }
    }

    fn peek_precedence(input: ParseStream) -> Precedence {
        if let Ok(op) = input.fork().parse() {
            Precedence::of(&op)
        } else if input.peek::<Token![=]>()
        /* && !input.peek::<Token![=>]>() */
        {
            Precedence::Assign
        // } else if input.peek(Token![..]) {
        //     Precedence::Range
        // } else if input.peek(Token![as])
        //     || cfg!(feature = "full") && input.peek(Token![:]) && !input.peek(Token![::])
        // {
        //     Precedence::Cast
        } else {
            Precedence::Any
        }
    }

    fn parse_expr(input: ParseStream, mut lhs: Expr, base: Precedence) -> Result<Expr> {
        loop {
            if input
                .fork()
                .parse::<BinOp>()
                .ok()
                .map_or(false, |op| Precedence::of(&op) >= base)
            {
                let op: BinOp = input.parse()?;
                let precedence = Precedence::of(&op);
                let mut rhs = unary_expr(input)?;
                loop {
                    let next = peek_precedence(input);
                    if next > precedence || next == precedence && precedence == Precedence::Assign {
                        rhs = parse_expr(input, rhs, next)?;
                    } else {
                        break;
                    }
                }
                lhs = if precedence == Precedence::Assign {
                    todo!()
                    // Expr::AssignOp(ExprAssignOp {
                    //     attrs: Vec::new(),
                    //     left: Box::new(lhs),
                    //     op,
                    //     right: Box::new(rhs),
                    // })
                } else {
                    Expr::Binary(ExprBinary {
                        lhs: Box::new(lhs),
                        op,
                        rhs: Box::new(rhs),
                    })
                };
            } else if Precedence::Assign >= base && input.peek::<Token![=]>() {
                let eq_token: Token![=] = input.parse()?;
                let mut rhs = unary_expr(input)?;
                loop {
                    let next = peek_precedence(input);
                    if next >= Precedence::Assign {
                        rhs = parse_expr(input, rhs, next)?;
                    } else {
                        break;
                    }
                }
                todo!()
                // lhs = Expr::Assign(ExprAssign {
                //     attrs: Vec::new(),
                //     left: Box::new(lhs),
                //     eq_token,
                //     right: Box::new(rhs),
                // });
                // } else if Precedence::Range >= base && input.peek(Token![..]) {
                //     let limits: RangeLimits = input.parse()?;
                //     let rhs = if input.is_empty()
                //         || input.peek(Token![,])
                //         || input.peek(Token![;])
                //         || input.peek(Token![.]) && !input.peek(Token![..])
                //         || !allow_struct.0 && input.peek(token::Brace)
                //     {
                //         None
                //     } else {
                //         let mut rhs = unary_expr(input)?;
                //         loop {
                //             let next = peek_precedence(input);
                //             if next > Precedence::Range {
                //                 rhs = parse_expr(input, rhs, allow_struct, next)?;
                //             } else {
                //                 break;
                //             }
                //         }
                //         Some(rhs)
                //     };
                //     lhs = Expr::Range(ExprRange {
                //         attrs: Vec::new(),
                //         from: Some(Box::new(lhs)),
                //         limits,
                //         to: rhs.map(Box::new),
                //     });
                // } else if Precedence::Cast >= base && input.peek(Token![as]) {
                //     let as_token: Token![as] = input.parse()?;
                //     let ty = input.call(Type::without_plus)?;
                //     check_cast(input)?;
                //     lhs = Expr::Cast(ExprCast {
                //         attrs: Vec::new(),
                //         expr: Box::new(lhs),
                //         as_token,
                //         ty: Box::new(ty),
                //     });
                // } else if Precedence::Cast >= base && input.peek(Token![:]) && !input.peek(Token![::]) {
                //     let colon_token: Token![:] = input.parse()?;
                //     let ty = input.call(Type::without_plus)?;
                //     check_cast(input)?;
                //     lhs = Expr::Type(ExprType {
                //         attrs: Vec::new(),
                //         expr: Box::new(lhs),
                //         colon_token,
                //         ty: Box::new(ty),
                //     });
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    #[derive(Clone, Copy, PartialEq, PartialOrd)]
    enum Precedence {
        Any,
        Assign,
        Range,
        Or,
        And,
        Compare,
        BitOr,
        BitXor,
        BitAnd,
        Shift,
        Arithmetic,
        Term,
        Exp,
        Cast,
    }

    impl Precedence {
        fn of(op: &BinOp) -> Self {
            match *op {
                BinOp::Add(_) | BinOp::Sub(_) => Precedence::Arithmetic,
                BinOp::Mul(_) | BinOp::Div(_) | BinOp::Rem(_) => Precedence::Term,
                BinOp::Exp(_) => Precedence::Exp,
                BinOp::And(_) => Precedence::And,
                BinOp::Or(_) => Precedence::Or,
                BinOp::Eq(_)
                | BinOp::Lt(_)
                | BinOp::Le(_)
                | BinOp::Ne(_)
                | BinOp::Ge(_)
                | BinOp::Gt(_) => Precedence::Compare,
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{syn::*, tokenizer::tokenize};

    use super::*;

    #[test]
    fn variable() {
        assert_parse_to! ("sys::ls", Expr(:: sys ls));
    }

    #[test]
    fn field() {
        assert_parse_to!("some::path.field", Expr(. (:: some path) field));
    }

    #[test]
    fn lit() {
        assert_parse_to!(r#""I am a string" + 42.69 + true"#, Expr(+ (+ "I am a string" 42.69) true));
    }

    #[test]
    fn function_call() {
        assert_parse_to! ("sys::ls(config.from_dir)", Expr((:: sys ls) ((. (:: config) from_dir))));
        assert_parse_to! (
            "move_and_link(config.source, config.sync, path)",
            Expr((:: move_and_link) ((. (:: config) source) (. (:: config) sync) (:: path)))
        );
    }

    #[test]
    fn pre_unary() {
        assert_parse_to!("! true", Expr(!true));
    }

    #[test]
    fn post_unary() {
        assert_parse_to!("true?", Expr(? true));
    }

    #[test]
    fn binary() {
        assert_parse_to!("1 + 2", Expr(+ 1 2));
    }

    #[test]
    fn if_() {
        assert_parse_to!("if true { true }", Expr(if true (true) ()));
        assert_parse_to!("if true { true } else { false }", Expr(if true (true) ((else (; (false))))));
        assert_parse_to!("if true { true } else if true {false}", Expr(if true (true) ((else (if true (false) ())))));
    }

    #[test]
    fn method_call() {
        assert_parse_to!("some::path.method(arg, 1)", Expr(. (:: some path) method ((:: arg) 1)));
    }

    #[test]
    fn index() {
        assert_parse_to!("array[20]", Expr([] (:: array) 20));
    }

    #[test]
    fn struct_() {
        assert_parse_to!("{test: 10}", Expr({} ((: test 10))));
    }

    #[test]
    fn tuple() {
        assert_parse_to!("(a, b)", Expr(, ((:: a) (:: b))));
        assert_parse_to!("(a, )", Expr(, ((:: a))));
    }

    #[test]
    fn paren() {
        assert_parse_to!("(a)", Expr(((::a))));
        assert_parse_to!("(a - b) * 10", Expr(* ((- (:: a) (:: b))) 10));
    }

    #[test]
    fn return_() {
        assert_parse_to!("return a", Expr(return ((:: a))));
    }

    #[test]
    fn array() {
        assert_parse_to!("[a, b, c]", Expr([]((::a)(::b)(::c))));
    }

    #[test]
    fn block() {
        assert_parse_to!("{true; false}", Expr(; (true false)));
    }
}
