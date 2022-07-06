#[macro_export]
macro_rules! expect {
    ($token:pat in $tokens:ident) => {
        match $tokens.next() {
            Some(ret @ ($token, _)) => ret,
            other => unexpected!(other, expected $token),
            // Some((wrong, span)) => fail!(span, "Expected {} found `{wrong:?}`", (stringify!($($token)?))),
            // _ => fail!(EOF, "Expected {} found EOF", (stringify!($($token)?))),
        }
    };
    ($token:pat in $tokens:ident => $res:expr) => {
        match $tokens.next() {
            Some($token) => $res,
            other => unexpected!(other, expected $token),
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
    (expected $expected:tt in $tokens:expr) => {
        if let Some(token) = $tokens.next() {
            fail!(token.1.clone(), "Expected `{}` found `{:?}`", (stringify!($expected)), (token.0))
        } else {
            fail!(0..0, "Expected `{}` found `{:?}`", (stringify!($expected)), "EOF")
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
        let current = $tokens.current();
        $(
            if let Ok($pat) = $tokens.parse() {
                return Ok($target);
            } else {
                $tokens.reset_to(current);
            }
        )*

    };
}
