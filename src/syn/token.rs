use super::{ParseDelimited, Span};

macro_rules! tokens {
    {$($token:tt $name:ident)*} => {
        $(
            #[derive(Debug)]
            pub struct $name {
                pub span: Span
            }

            impl $crate::syn::Parse for $name {
                fn parse(tokens: &mut $crate::tokenizer::ParseBuffer) -> $crate::syn::Result<Self> {
                    Ok(Self{span: expect!($crate::tokenizer::TokenKind::$name, span in tokens => *span)})
                }
            }

            impl $crate::syn::lisp::ToLisp for $name {
                fn to_lisp(&self) -> $crate::syn::lisp::Lisp {
                    lisp!(stringify!($token))
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
                    tokens: &mut crate::tokenizer::ParseBuffer<'source>,
                ) -> super::Result<(Self, crate::tokenizer::ParseBuffer<'source>)> {
                    match tokens.next() {
                        Some(Ok($crate::tokenizer::ParseToken::Group{start, end, inner})) if start.kind == $crate::tokenizer::TokenKind::$start => Ok((Self{span: start.span + end.span}, inner)),
                        Some(Ok($crate::tokenizer::ParseToken::Group{start, ..})) => unexpected!(Some(start), expected $start),
                        Some(Err(e)) => return Err(e.clone()),
                        Some(Ok($crate::tokenizer::ParseToken::Normal(normal))) => unexpected!(Some(normal), expected $start),
                        None => unexpected!(&None, expected $start)
                    }
                    // let mut start = expect!($crate::tokenizer::TokenKind::$start, span in tokens => *span);
                    // let mut open = 0;
                    //
                    // let end = tokens.tokens.iter().position(|token| {
                    //     match &token {
                    //         $crate::tokenizer::Token{kind:$crate::tokenizer::TokenKind::$start, ..} => open += 1,
                    //         $crate::tokenizer::Token{kind:$crate::tokenizer::TokenKind::$end, ..} if open == 0 => return true,
                    //         $crate::tokenizer::Token{kind:$crate::tokenizer::TokenKind::$end, ..} => open -= 1,
                    //         _ => ()
                    //     }
                    //     false
                    // }).ok_or_else(|| error!(start, "Missing {} for {}", (stringify!($end)), (stringify!($start))))?;
                    //
                    // start.extend(tokens.tokens[end].span);
                    //
                    // let inner = &tokens.tokens[0..end];
                    //
                    // tokens.tokens = &tokens.tokens[end+1..];
                    //
                    // Ok((Self{ span: start }, $crate::tokenizer::ParseBuffer{tokens: inner}))
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
                token
            }
            Err(e) => return Err(e),
        }
    };
}
