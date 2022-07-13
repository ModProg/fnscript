#[macro_export]
macro_rules! expect {
    // ($token:pat in $tokens:ident) => {
    //     match $tokens.next() {
    //         Some($token) => ret,
    //         other => unexpected!(other, expected $token),
    //         // Some((wrong, span)) => fail!(span, "Expected {} found `{wrong:?}`", (stringify!($($token)?))),
    //         // _ => fail!(EOF, "Expected {} found EOF", (stringify!($($token)?))),
    //     }
    // };
    ($token:pat, $span:ident = $tokens:expr => $res:expr) => {
        match $tokens {
            Some($crate::tokenizer::Token{ kind:$token, span: $span }) => $res,
            other => unexpected!(other, expected $token),
            // Some((wrong, span)) => fail!(span, "Expected {} found `{wrong:?}`", (stringify!($($token)?))),
            // _ => fail!(EOF, "Expected {} found EOF", (stringify!($($token)?))),
        }
    };
    ($token:pat, $span:ident in $tokens:ident => $res:expr) => {
        match $tokens.next() {
            Some($crate::tokenizer::Token{ kind:$token, span: $span }) => $res,
            other => unexpected!(other, expected $token),
            // Some((wrong, span)) => fail!(span, "Expected {} found `{wrong:?}`", (stringify!($($token)?))),
            // _ => fail!(EOF, "Expected {} found EOF", (stringify!($($token)?))),
        }
    };
}

macro_rules! unexpected {
    ($token:expr, expected $($expected:tt)*) => {
        if let Some($crate::tokenizer::Token{kind, span}) = $token {
            fail!(*span, "Expected `{}` found `{:?}`", (stringify!($($expected)*)), kind)
        } else {
            fail!($crate::tokenizer::Span::EOF, "Expected `{}` found `{:?}`", (stringify!($($expected)*)), "EOF")
        }
    };
    (expected $expected:tt in $tokens:expr) => {
        if let Some($crate::tokenizer::Token{kind, span}) = $tokens.next() {
            fail!(*span, "Expected `{}` found `{:?}`", (stringify!($expected)), kind)
        } else {
            fail!($crate::tokenizer::Span::EOF, "Expected `{}` found `{:?}`", (stringify!($expected)), "EOF")
        }
    };
}

macro_rules! error {
    (EOF, $($tts:tt),* $(,)?) => {
        {$crate::syn::Error::eof(format!($($tts),*))}
    };
    (internal, $($tts:tt),* $(,)?) => {
        {$crate::syn::Error::eof(format!("INTERNAL: {}", format_args!($($tts),*)))}
    };
    ($span:expr, $($tts:tt),* $(,)?) => {
        {$crate::syn::Error::new($span, format!($($tts),*))}
    };
}

macro_rules! fail {
    (EOF, $($tts:tt),* $(,)?) => {
        {return Err(error!(EOF, $($tts),*));}
    };
    (internal, $($tts:tt),* $(,)?) => {
        {return Err(error!(internal, $($tts),*));}
    };
    ($span:expr, $($tts:tt),* $(,)?) => {
        {return Err(error!($span, $($tts),*));}
    };
}

macro_rules! any {
    {
        $tokens:ident:
        $($target:path),*
        $(,)?
    } => {
        any! {
            $tokens:
                $(value => $target(value)),*
        }

    };
    {
        $tokens:ident:
        $($pat:pat => $target:expr),*
        $(,)?
    } => {
        let current = $tokens.clone();
        $(
            if let Ok($pat) = $tokens.parse() {
                return Ok($target);
            } else {
                *$tokens = current;
            }
        )*

    };
}

macro_rules! defer {
    ($in:expr; $($vars:pat),* => $expr:expr) => {
        match $in {
            $($vars => $expr),*
        }
    };
}
