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

impl Expression {
    fn parse(tokens: &mut TokenStream, precedence: ExpressionPrecedence) -> Result<Self> {
        use ExpressionPrecedence as Precedence;
        if let Precedence::Binary(_) = precedence {
            let current = tokens.current();
            if let Ok(value) = Binary::parse(tokens, precedence) {
                return Ok(value);
            } else {
                tokens.reset_to(current);
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

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
enum ExpressionPrecedence {
    Lit,
    Binary(BinaryPrecedence),
}

impl ExpressionPrecedence {
    fn next(self) -> Self {
        use BinaryPrecedence::*;
        use ExpressionPrecedence::*;
        match self {
            Binary(binary) => Binary(match binary {
                Or => And,
                And => Compare,
                Compare => Arithmetic,
                Arithmetic => Term,
                Term => return Lit,
            }),
            PostUnary => Lit,
            Lit => Lit,
        }
    }
}

// TODO make sure this is the correct order ARFGG
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
enum BinaryPrecedence {
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
    // Cast,
    // Eq(Token![==]),
    // NotEq(Token![!=]),
    // Lt(Token![<]),
    // LtEq(Token![<=]),
    // Gt(Token![>]),
    // GtEq(Token![>=]),
    // Minus(Token![-]),
    // And(Token![&&]),
    // Or(Token![||]),
    // Plus(Token![+]),
    // Mul(Token![*]),
    // Div(Token![/]),
    // Exp(Token![^]),
    // Mod(Token![%]),
}

impl Parse for Expression {
    fn parse(tokens: &mut TokenStream) -> Result<Self> {
        Expression::parse(tokens, ExpressionPrecedence::Binary(BinaryPrecedence::Or))
    }
}

#[derive(Debug)]
pub enum Lit {
    String(LitStr),
    Number(LitNumber),
}

impl Parse for Lit {
    fn parse(tokens: &mut TokenStream) -> Result<Self> {
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
    fn parse(tokens: &mut TokenStream) -> Result<Self> {
        expect! {
            (Token::String(value), span) in tokens =>
                Ok(Self{value: value.to_string(), span: span.clone()})
        }
    }
}

#[derive(Debug)]
pub struct LitNumber {
    value: f64,
    span: Span,
}

impl Parse for LitNumber {
    fn parse(tokens: &mut TokenStream) -> Result<Self> {
        expect! {
            (Token::Number(value), span) in tokens =>
                Ok(Self{value: *value, span: span.clone()})
        }
    }
}

#[derive(Debug)]
pub struct FunctionCall {
    path: Path,
    paren: Paren,
    args: Vec<Expression>,
}

impl Parse for FunctionCall {
    fn parse(tokens: &mut TokenStream) -> Result<Self> {
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

impl Parse for PostUnary {
    fn parse(tokens: &mut TokenStream) -> Result<Self> {
        let tokens = &mut &mut tokens.clone();
        Ok(Self {
            op: {
                let (token, span) = tokens
                    .peek_back()
                    .ok_or_else(|| error!(internal, "Expected end"))?
                    .to_owned();
                match token {
                    Token::Question => {
                        tokens.next_back();
                        PostUnaryOp::Question(Question { span })
                    }
                    _ => fail!(internal, "not a PostUnary"),
                }
            },
            expr: Box::new(Expression::parse(tokens, ExpressionPrecedence::Lit)?),
        })
    }
}

#[derive(Debug)]
pub enum PostUnaryOp {
    Question(Question),
    Bang(Bang),
}

impl Parse for PostUnaryOp {
    fn parse(tokens: &mut TokenStream) -> Result<Self> {
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

impl Binary {
    fn parse(tokens: &mut TokenStream, precedence: ExpressionPrecedence) -> Result<Expression> {
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

impl BinaryOp {
    fn parse(tokens: &mut TokenStream, precedence: ExpressionPrecedence) -> Result<Self> {
        use BinaryOp::*;
        Ok(match precedence {
            ExpressionPrecedence::Binary(precedence) => match precedence {
                BinaryPrecedence::Or => Or(tokens.parse()?),
                BinaryPrecedence::And => And(tokens.parse()?),
                BinaryPrecedence::Compare => {
                    any! {tokens:
                        Eq,NotEq,Lt,LtEq,Gt,GtEq
                    }
                    unexpected!(expected Comparison in tokens)
                }
                BinaryPrecedence::Arithmetic => {
                    any! {tokens:
                        Plus, Minus
                    }
                    unexpected!(expected "`+` or `-`" in tokens)
                }
                BinaryPrecedence::Term => {
                    any! {tokens:
                        Mul, Div
                    }
                    unexpected!(expected "`*` or `/`" in tokens)
                }
            },
            _ => unreachable!("Only valid for binary expressions"),
        })
    }
}
