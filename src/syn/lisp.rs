pub use fns_macros::ToLisp;
use std::fmt::{Debug, Display};

macro_rules! lisp {
    [($($tts:tt)*)] => {
        Lisp::Multiple(vec![$(lisp!($tts)),*])
    };
    ({$expr:expr}) => {
        $expr.to_lisp()
    };
    ($expr:expr) => {
        $expr.to_lisp()
    };
}

#[allow(unused)]
macro_rules! lispm {
    [($($tts:tt)*)] => {
        Lisp::Multiple(vec![$(lispm!($tts)),*])
    };
    [$tt:tt] => {
        Lisp::Single(stringify!($tt).to_string())
    };
    [$($tts:tt)*] => {
        Lisp::Multiple(vec![$(lispm!($tts)),*])
    };
}

pub trait ToLisp {
    fn to_lisp(&self) -> Lisp;
}

#[derive(PartialEq)]
pub enum Lisp {
    Single(String),
    Multiple(Vec<Lisp>),
}

impl Debug for Lisp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Single(arg0) => Display::fmt(arg0, f),
            Self::Multiple(arg0) => arg0.fmt(f),
        }
    }
}

impl<T: ToLisp> ToLisp for Vec<T> {
    fn to_lisp(&self) -> Lisp {
        Lisp::Multiple(self.iter().map(ToLisp::to_lisp).collect())
    }
}

impl ToLisp for str {
    fn to_lisp(&self) -> Lisp {
        Lisp::Single(self.to_string())
    }
}

impl ToLisp for u64 {
    fn to_lisp(&self) -> Lisp {
        Lisp::Single(self.to_string())
    }
}

impl<T: ToLisp> ToLisp for Option<T> {
    fn to_lisp(&self) -> Lisp {
        match self {
            Some(inner) => lisp![(inner)],
            None => lisp![()],
        }
    }
}

impl<T: ToLisp> ToLisp for Box<T> {
    fn to_lisp(&self) -> Lisp {
        // We call it on the inner of Box so to say
        (**self).to_lisp()
    }
}
