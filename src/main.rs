extern crate docopt;
extern crate env_logger;
#[macro_use]
extern crate log;
extern crate rustc_serialize;

extern crate reduced_c_syntax as syntax;

use docopt::Docopt;
use std::fs::File;
use std::io::{self, Read, Write, BufReader};
use std::process;

pub mod instr;
pub mod trans;
pub mod opt;

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
                println!("Failed to read {}: {}", args.arg_src, e);
                return;
            }
        }
    };

    let ast = match syntax::parse(&mut BufReader::new(infile)) {
        Ok(t) => t,
        Err(e) => {
            write!(stderr, "{}", e).unwrap();
            process::exit(1);
        }
    };

    let mut assembly = trans::compile(ast);
    opt::optimize(&mut assembly);
    print_assembly(&assembly, io::stdout()).unwrap();
}

fn print_assembly<W: Write>(program: &[(instr::Label, instr::Instruction)], mut dest: W) -> io::Result<()> {
    for &(ref label, ref instruction) in program {
        if let &instr::Label::Name(_) = label {
            try!(write!(dest, "{}:\n", label));
        }
        match instruction {
            &instr::Instruction::Nop => { /* nops are only ever generated as filler */ }
            i => try!(write!(dest, "    {}\n", i))
        }
    }
    Ok(())
}
