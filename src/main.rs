use scheme_rust::repl;
use std::io;

fn main() {
    let stdin = io::stdin();
    let stdin = stdin.lock();
    let reader = io::BufReader::new(stdin);
    let writer = io::BufWriter::new(io::stdout());
    repl::start(reader, writer).unwrap();
}
