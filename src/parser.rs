// use nom::{
//     branch::*, bytes::complete::*, character::complete::satisfy, combinator::*, error::Error,
//     multi::*, sequence::pair, IResult, InputTakeAtPosition, Parser,
// };
// use nom_supreme::{error::ErrorTree, ParserExt};
// use unicode_xid::UnicodeXID;
//
// use crate::script::*;
//
// type I<'s> = &'s str;
// type Result<'s, T> = IResult<&'s str, T, ErrorTree<&'s str>>;
//
// pub fn parse_script(script: I) -> Result<Vec<Function>> {
//     many1(function)(script)
// }
//
// fn function(s: I) -> Result<Function> {
//     let (s, vis) = opt(vis)(s)?;
//     let (s, _) = tag("fn")(s)?;
//     let (s, ident) = identifier(s)?;
//
//     todo!()
// }
//
// fn vis(s: I) -> Result<Vis> {
//     use Vis::*;
//     alt((
//         tag("pre").value(Pre),
//         tag("post").value(Post),
//         tag("pub").value(Pub),
//         tag("hid").value(Hid),
//     ))(s)
// }
//
// fn identifier(s: I) -> Result<Identifier> {
//     recognize(pair(
//         satisfy(|c| c.is_xid_start()),
//         take_while(|c: char| c.is_xid_continue()),
//     ))
//     .map(Identifier)
//     .parse(s)
// }
