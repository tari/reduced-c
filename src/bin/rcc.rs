extern crate docopt;
extern crate env_logger;
#[macro_use]
extern crate log;
extern crate rustc_serialize;

extern crate reduced_c as compiler;

use docopt::Docopt;
use std::fs::File;
use std::io::{self, Read, Write, BufReader};
use std::process;

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
    
    let ref mut stderr = io::stderr();
    let infile: Box<Read> = if args.arg_src == "-" {
        Box::new(io::stdin())
    } else {
        match File::open(&args.arg_src) {
            Ok(f) => Box::new(f),
            Err(e) => {
                write!(stderr, "Failed to read {}: {}\n", args.arg_src, e).unwrap();
                process::exit(1);
            }
        }
    };

    match compiler::compile(BufReader::new(infile)) {
        Err(e) => {
            write!(stderr, "{}\n", e).unwrap();
            process::exit(1);
        }
        Ok(assembly) => {
            compiler::print_assembly(&assembly, io::stdout()).unwrap();
        }
    }
}
