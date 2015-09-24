extern crate docopt;
extern crate env_logger;
#[macro_use]
extern crate log;
extern crate rustc_serialize;

extern crate reduced_c_syntax as syntax;

use docopt::Docopt;
use std::fs::File;
use std::io::{Read, Write, BufReader};
use std::process;

pub mod instr;
pub mod trans;

static USAGE: &'static str = "
Usage: rcc [options] <src>
       rcc --help
       rcc --version

Options:
    -o FILE     Write output to FILE instead of stdout.
";

#[derive(RustcDecodable, Debug)]
#[allow(non_snake_case)]
struct Args {
    arg_src: String,
    flag_o: Option<String>,
}

pub fn main() {
    env_logger::init().unwrap();
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.version(Some("0.1.0".to_string())).decode())
        .unwrap_or_else(|e| e.exit());
    debug!("{:?}", args);
    
    let ref mut stderr = std::io::stderr();
    let infile: Box<Read> = if args.arg_src == "-" {
        Box::new(std::io::stdin())
    } else {
        match File::open(&args.arg_src) {
            Ok(f) => Box::new(f),
            Err(e) => {
                println!("Failed to read {}: {}", args.arg_src, e);
                return;
            }
        }
    };

    let ast = match syntax::parse(&mut BufReader::new(infile)) {
        Ok(t) => t,
        Err(e) => {
            write!(stderr, "Parse error: {}", e).unwrap();
            process::exit(1);
        }
    };

    let assembly = trans::compile(ast);
    println!("{:?}", assembly);
}

