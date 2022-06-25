use super::{ParseDelimited, Span};

macro_rules! expect {
    ($tokens:ident, $($token:tt)*) => {
        match $tokens.next() {
            Some(ret @ ($($token)*, _)) => ret,
            other => unexpected!(other, expected $($token)*),
            // Some((wrong, span)) => fail!(span, "Expected {} found `{wrong:?}`", (stringify!($($token)?))),
            // _ => fail!(EOF, "Expected {} found EOF", (stringify!($($token)?))),
        }
    };
}

macro_rules! unexpected {
    ($token:expr, expected $($expected:tt)*) => {
        if let Some(token) = $token {
            fail!(token.1.clone(), "Expected `{}` found `{:?}`", (stringify!($($expected)*)), (token.0))
        } else {
            fail!(0..0, "Expected `{}` found `{:?}`", (stringify!($($expected)*)), "EOF")
        }
    };
}

macro_rules! fail {
    (EOF, $($tts:tt),* $(,)?) => {
        {return Err($crate::syn::Error::eof(format!($($tts),*)));}
    };
    ($span:expr, $($tts:tt),* $(,)?) => {
        {return Err($crate::syn::Error::new($span, format!($($tts),*)));}
    };
}

macro_rules! tokens {
    {$($token:tt $name:ident)*} => {
        $(
            #[derive(Debug)]
            pub struct $name {
                span: Span
            }

            impl $crate::syn::Parse for $name {
                fn parse(tokens: &mut $crate::tokenizer::TokenStream) -> $crate::syn::Result<Self> {
                    Ok(Self{span: expect!(tokens, $crate::tokenizer::Token::$name).1.clone()})
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
    hid  Hid
    pub  Pub
    pre  Pre
    post Post

    // Punctuation
    : Colon
    , Comma
    ; Semi
    * Mul
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

                    let start = expect!(tokens, $crate::tokenizer::Token::$start).1.clone();
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
    ($tokens:ident, $content:ident) => {
        match $tokens.parse_delimited() {
            Ok((token, content)) => {
                $content = content;
                Ok(token)
            }
            Err(e) => return Err(e),
        }
    };
}
