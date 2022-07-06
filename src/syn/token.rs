use super::{ParseDelimited, Span};

macro_rules! tokens {
    {$($token:tt $name:ident)*} => {
        $(
            #[derive(Debug)]
            pub struct $name {
                pub span: Span
            }

            impl $crate::syn::Parse for $name {
                fn parse(tokens: &mut $crate::tokenizer::TokenStream) -> $crate::syn::Result<Self> {
                    Ok(Self{span: expect!($crate::tokenizer::Token::$name in tokens).1.clone()})
                }
            }
        )*
        macro_rules! Token {
            $(($token) => {$crate::syn::token::$name});*
        }
    };
}

tokens! {
    fn   Fn
    let  Let
    in   In
    hid  Hid
    pub  Pub
    pre  Pre
    post Post

    // Punctuation
    :   Colon
    ::  PathSep
    ,   Comma
    ;   Semi
    =   Eq
    ?   Question
    !   Bang
    ==  EqEq
    !=  NotEq
    <   Lt
    <=  LtEq
    >   Gt
    >=  GtEq
    &&  And
    ||  Or
    +   Plus
    -   Minus
    *   Asterix
    /   Div
    ^   Carret
    %   Percent
}

macro_rules! delimiters {
    {$($start:ident $end:ident $name:ident)*} => {
        $(
            #[derive(Debug)]
            pub struct $name {
                pub span: Span
            }
            impl ParseDelimited for $name {
                fn parse_delimited<'source>(
                    tokens: &mut crate::tokenizer::TokenStream<'source>,
                ) -> super::Result<(Self, crate::tokenizer::TokenBuffer<'source>)> {
                    let mut inner = Vec::new();

                    let start = expect!($crate::tokenizer::Token::$start in tokens).1.clone();
                    let mut open = 0;

                    let end = loop {
                        let token = tokens.next();
                        match &token {
                            Some(($crate::tokenizer::Token::$start, _)) => open += 1,
                            Some(($crate::tokenizer::Token::$end, span)) if open == 0 => break span,
                            Some(($crate::tokenizer::Token::$end, _)) => open -= 1,
                            None => fail!(start, "Missing {} for {}", (stringify!($end)), (stringify!($start))),
                            _ => ()
                        }
                        inner.push(token.cloned().expect("fail on EOF"));
                    };

                    Ok((Self{
                        span: Span{
                            start:start.start,end:end.end
                        }
                    }, inner.into_iter().collect()))
                }
            }
        )*
    };
}

delimiters! {
    ParenOpen ParenClose Paren
    BraceOpen BraceClose Brace
    BracketOpen BracketClose Bracket
}

macro_rules! delimited {
    ($content:ident in $tokens:ident) => {
        match $tokens.parse_delimited() {
            Ok((token, content)) => {
                $content = content;
                Ok(token)
            }
            Err(e) => return Err(e),
        }
    };
}
