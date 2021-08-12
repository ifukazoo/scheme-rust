use scheme_rust::error::SRError;
use scheme_rust::eval;
use scheme_rust::lexer;
use scheme_rust::object::Object;
use scheme_rust::parser;
use std::io;
use std::io::BufReader;
use std::io::Read;

fn main() {
    let stdin = io::stdin();
    let stdin = stdin.lock();
    let reader = io::BufReader::new(stdin);
    let result = run(reader);
    match result {
        Ok(obj) => println!("{}", obj),
        Err(e) => println!("{}", e),
    }
}
pub fn run<R: Read>(mut reader: BufReader<R>) -> Result<Object, SRError> {
    let mut buf = String::new();
    reader.read_to_string(&mut buf)?;
    let token = lexer::lex(&buf)?;
    let element = parser::parse_program(token)?;
    let obj = eval::eval_program(element)?;
    Ok(obj)
}
