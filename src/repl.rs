use crate::*;
use std::collections::HashMap;
use std::io::BufRead;
use std::io::BufReader;
use std::io::BufWriter;
use std::io::Read;
use std::io::Write;

pub fn start<R: Read, W: Write>(
    mut reader: BufReader<R>,
    mut writer: BufWriter<W>,
) -> std::io::Result<()> {
    let prompt = ">>";
    let env = env::new_env(HashMap::new());
    loop {
        writer.write_all(prompt.as_bytes())?;
        writer.flush()?;
        let mut line = String::new();
        let size = reader.read_line(&mut line)?;
        if size == 0 {
            // EOF
            break;
        }
        let lex_result = match lexer::lex(&line) {
            Ok(r) => r,
            Err(e) => {
                writer.write_all(format!("{:?}\n", e).as_bytes())?;
                continue;
            }
        };
        match parser::parse_program(lex_result) {
            Ok(program) => match eval::eval(&program, &env) {
                Ok(obj) => {
                    writer.write_all(format!("{}\n", obj).as_bytes())?;
                }
                Err(e) => {
                    writer.write_all(format!("{:?}\n", e).as_bytes())?;
                }
            },
            Err(e) => {
                writer.write_all(format!("{:?}\n", e).as_bytes())?;
            }
        }
    }
    Ok(())
}
