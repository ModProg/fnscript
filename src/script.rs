pub struct Instruction;
pub struct Function;
pub struct Identifier<'s>(pub &'s str);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Vis {
    Pre,
    Post,
    Pub,
    Hid,
    Priv
}
