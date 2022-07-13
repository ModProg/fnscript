use super::*;
// Array
// Binary
// Call
// Field
// (Group)
// If
// Index
// Literal
// MethodCall
// Paren
// Variable
// (Range)
// Struct
// Tuple
// Unary
//

// 13.2 + a * c / 5 ? + 2 > 4
//      13.2
//  +
//                          a
//                      *
//                          c
//                  /
//                      5
//              /
//                  5 ?
//          *
//              4
//      +
//          2
// >
//  4

#[derive(Debug)]
pub enum Expression {
    Variable(Path),
    Lit(Lit),
    FunctionCall(FunctionCall),
    PostUnary(PostUnary),
    Binary(Binary),
}

impl ToLisp for Expression {
    fn to_lisp(&self) -> Lisp {
        use Expression::*;
        defer!(self; 
            Variable(v), Lit(v), FunctionCall(v), PostUnary(v), Binary(v) 
            => lisp!(v))
    }
}

impl Expression {
    fn parse(tokens: &mut ParseBuffer, precedence: Precedence) -> Result<Self> {
        let current = *tokens;
        if precedence.is_binary() {
            if let Ok(value) = Binary::parse(tokens, precedence) {
                return Ok(value);
            } else {
                *tokens = current;
            }
        }
        any! {tokens:
            Self::PostUnary,
            Self::FunctionCall,
            Self::Lit,
            Self::Variable,
        }

        unexpected!(expected Expression in tokens);
    }
}

// TODO make sure this is the correct order ARFGG
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
enum Precedence {
    // ||
    Or,
    // &&
    And,
    // == < > >= <= !=
    Compare,
    // + -
    Arithmetic,
    // * /
    Term,
    // !
    Prefix,
    // ? ()
    Postfix,
    Lit,
}
impl Precedence {
    fn next(&self) -> Precedence {
        use Precedence::*;
        match self {
            Or => And,
            And => Compare,
            Compare => Arithmetic,
            Arithmetic => Term,
            Term => Prefix,
            Prefix => Postfix,
            Postfix => Lit,
            Lit => unreachable!("What are you doing here"),
        }
    }

    fn is_binary(self) -> bool {
        self < Self::Prefix
    }
}

impl Parse for Expression {
    fn parse(tokens: &mut ParseBuffer) -> Result<Self> {
        Expression::parse(tokens, Precedence::Or)
    }
}

#[derive(Debug)]
pub enum Lit {
    String(LitStr),
    Number(LitNumber),
}

impl ToLisp for Lit {
    fn to_lisp(&self) -> Lisp {
        match self {
            Lit::String(string) => lisp!(format!("{:?}", string.value)),
            Lit::Number(number) => lisp!(number.value.to_string())
        }
    }
}

impl Parse for Lit {
    fn parse(tokens: &mut ParseBuffer) -> Result<Self> {
        use Lit::*;
        any! {tokens:
            String, Number
        }
        unexpected!(expected Literal in tokens)
    }
}

// TODO string expansion + raw strings
#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
pub struct FunctionCall {
    path: Path,
    paren: Paren,
    args: Vec<Expression>,
}

impl ToLisp for FunctionCall {
    fn to_lisp(&self) -> Lisp {
        let Self { path,  args, .. } = self;
        lisp![(path args)]
    }
}

impl Parse for FunctionCall {
    fn parse(tokens: &mut ParseBuffer) -> Result<Self> {
        let mut inner;
        Ok(Self {
            path: tokens.parse()?,
            paren: delimited!(inner in tokens)?,
            args: (&mut inner).parse_seperated::<_, Token![,]>()?,
        })
    }
}

#[derive(Debug)]
pub struct PostUnary {
    expr: Box<Expression>,
    op: PostUnaryOp,
}

impl ToLisp for PostUnary {
    fn to_lisp(&self) -> Lisp {
        let Self{expr, op} = self;
        lisp![(op expr)]
    }
}

#[derive(Debug)]
pub struct FnCall {
    span: Span,
}

impl Parse for PostUnary {
    fn parse(tokens: &mut ParseBuffer) -> Result<Self> {
        Ok(Self {
            op: {
                let Token { span, kind } = tokens
                    .next_back()
                    .ok_or_else(|| error!(internal, "Expected end"))?
                    .to_owned();
                match kind {
                    TokenKind::Question => PostUnaryOp::Question(Question { span }),
                    // TokenKind::ParenClose => {
                    //     let inner_span = expect!(
                    //         TokenKind::ParenOpen,
                    //         inner_span = tokens.next_back() =>
                    //         inner_span
                    //     );
                    //     span.extend(*inner_span);
                    //     PostUnaryOp::FnCall(FnCall { span })
                    // }
                    _ => fail!(internal, "not a PostUnary"),
                }
            },
            expr: Box::new(Expression::parse(tokens, Precedence::Postfix)?),
        })
    }
}

#[derive(Debug)]
pub enum PostUnaryOp {
    Question(Question),
    Bang(Bang),
}
impl ToLisp for PostUnaryOp {
    fn to_lisp(&self) -> Lisp {
        use PostUnaryOp::*;
        defer!(self; Question(v), Bang(v) => lisp!(v))
    }
}

impl Parse for PostUnaryOp {
    fn parse(tokens: &mut ParseBuffer) -> Result<Self> {
        use PostUnaryOp::*;
        any! {tokens:
            Question,
            Bang
        }

        unexpected!(expected PostUnaryOperator in tokens)
    }
}

#[derive(Debug)]
pub struct Binary {
    lhs: Box<Expression>,
    op: BinaryOp,
    rhs: Box<Expression>,
}

impl ToLisp for Binary {
    fn to_lisp(&self) -> Lisp {
        let Self { lhs, op, rhs } = self;
        lisp![(op lhs rhs)]
    }
}

impl Binary {
    fn parse(tokens: &mut ParseBuffer, precedence: Precedence) -> Result<Expression> {
        let ops: Vec<_> = iter::once(Ok((None, Expression::parse(tokens, precedence.next())?)))
            .chain(iter::from_fn(|| {
                tokens.peek().is_some().then(|| {
                    let op = BinaryOp::parse(tokens, precedence)?;
                    let expr = Expression::parse(tokens, precedence.next())?;
                    Ok((Some(op), expr))
                })
            }))
            .collect::<Result<_>>()?;
        let mut ops = ops.into_iter().rev();
        let (op, rhs) = ops.next().expect("There is one entry");
        if let Some(op) = op {
            Ok(*ops
                .fold((Some(op), Box::new(rhs)), |(op, rhs), (next_op, lhs)| {
                    let op = op.expect("there should always be an operator");
                    (
                        next_op,
                        Box::new(Expression::Binary(Self {
                            lhs: Box::new(lhs),
                            op,
                            rhs,
                        })),
                    )
                })
                .1)
        } else {
            fail!(internal, "only one expression in binary")
        }
    }
}

#[derive(Debug)]
pub enum BinaryOp {
    Eq(Token![==]),
    NotEq(Token![!=]),
    Lt(Token![<]),
    LtEq(Token![<=]),
    Gt(Token![>]),
    GtEq(Token![>=]),
    Minus(Token![-]),
    And(Token![&&]),
    Or(Token![||]),
    Plus(Token![+]),
    Mul(Token![*]),
    Div(Token![/]),
    Exp(Token![^]),
    Mod(Token![%]),
}
impl ToLisp for BinaryOp {
    fn to_lisp(&self) -> Lisp {
        use BinaryOp::*;
        defer!(self;
                Eq(v), NotEq(v), Lt(v), LtEq(v), Gt(v), GtEq(v), Minus(v), And(v), Or(v), Plus(v), Mul(v), Div(v), Exp(v), Mod(v)
                => lisp!(v)
            )
    }
}

impl BinaryOp {
    fn parse(tokens: &mut ParseBuffer, precedence: Precedence) -> Result<Self> {
        use BinaryOp::*;
        Ok(match precedence {
            Precedence::Or => Or(tokens.parse()?),
            Precedence::And => And(tokens.parse()?),
            Precedence::Compare => {
                any! {tokens:
                    Eq,NotEq,Lt,LtEq,Gt,GtEq
                }
                unexpected!(expected Comparison in tokens)
            }
            Precedence::Arithmetic => {
                any! {tokens:
                    Plus, Minus
                }
                unexpected!(expected "`+` or `-`" in tokens)
            }
            Precedence::Term => {
                any! {tokens:
                    Mul, Div
                }
                unexpected!(expected "`*` or `/`" in tokens)
            }
            _ => fail!(internal, "no binary expression"),
        })
    }
}
