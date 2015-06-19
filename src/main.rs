extern crate docopt;
#[macro_use]
extern crate log;
extern crate parser_combinators;
extern crate rustc_serialize;

use docopt::Docopt;
use std::fs::File;
use std::io::{Read, BufReader};

pub mod instr;
pub mod syntax;
pub mod trans;

static USAGE: &'static str = "
Usage: rcc [options] <src>
       rcc --help
       rcc --version

Options:
    -o FILE     Write output to FILE instead of stdout.
    -D          Emit debug messages to stderr.
";

#[derive(RustcDecodable, Debug)]
#[allow(non_snake_case)]
struct Args {
    arg_src: String,
    flag_o: Option<String>,
    flag_D: bool
}

fn main() {
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.version(Some("0.1.0".to_string())).decode())
        .unwrap_or_else(|e| e.exit());

    let loglevel = if args.flag_D {
        log::LogLevel::Debug
    } else {
        log::LogLevel::Info
    };
    StdioLogger::register(loglevel);
    debug!("{:?}", args);
    
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
            println!("{}", e);
            return;
        }
    };
}

struct StdioLogger(log::LogLevel);

impl log::Log for StdioLogger {
    fn enabled(&self, metadata: &log::LogMetadata) -> bool {
        metadata.level() <= self.0
    }

    fn log(&self, record: &log::LogRecord) {
        // TODO write to stderr
        if self.enabled(record.metadata()) {
            println!("{}: {}: {}", record.level(), record.location().module_path(), record.args());
        }
    }
}

impl StdioLogger {
    fn register(level: log::LogLevel) {
        log::set_logger(|max_log_level| {
            max_log_level.set(level.to_log_level_filter());
            Box::new(StdioLogger(level))
        }).unwrap();
    }
}

