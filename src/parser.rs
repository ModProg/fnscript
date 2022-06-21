use nom::IResult;

use crate::script::Instruction;

pub fn parse_script(script: &str) -> IResult<&str, Vec<Instruction>> {
    
}
