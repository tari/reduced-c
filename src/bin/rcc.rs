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

mod logger {
    use ::log::{LogRecord, LogLevel, LogMetadata, SetLoggerError, LogLevelFilter};
    struct StdoutLogger(LogLevel);

    impl ::log::Log for StdoutLogger {
        fn enabled(&self, metadata: &LogMetadata) -> bool {
            metadata.level() <= self.0
        }

        fn log(&self, record: &LogRecord) {
            if self.enabled(record.metadata()) {
                println!("{} - {}", record.level(), record.args());
            }
        }
    }

    pub fn init(ll: LogLevelFilter) -> Result<(), SetLoggerError> {
        ::log::set_logger(|max_log_level| {
            max_log_level.set(ll);
            Box::new(StdoutLogger(ll.to_log_level().unwrap()))
        })
    }
}

static USAGE: &'static str = "
Usage: rcc [options] <src>
       rcc --help
       rcc --version

Options:
    -o FILE         Write output to FILE instead of stdout.
    -l LOGLEVEL     Increase verbosity of output, useful for debugging.
                    LOGLEVEL is one of Error, Warn, Info, Debug or Trace
                    (Default: Error).
";

#[derive(RustcDecodable, Debug)]
#[allow(non_snake_case)]
struct Args {
    arg_src: String,
    flag_o: Option<String>,
    flag_l: Option<String>,
}

pub fn main() {
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.version(Some("0.1.0".to_string())).decode())
        .unwrap_or_else(|e| e.exit());
    debug!("{:?}", args);
    
    if cfg!(target_arch="asmjs") {
        // Logging in javascript is a mess, so we'll just spew to stdout.
        // (Environment variables don't exist, for one.)
        use std::str::FromStr;
        let ll = args.flag_l.map_or(log::LogLevelFilter::Error, |ref s| {
            FromStr::from_str(s).expect("Invalid log level")
        });
        logger::init(ll).unwrap();
    } else {
        // If not javascript, we can use env vars. Note -l LEVEL will override
        // any existing setting.
        std::env::set_var("RUST_LOG", args.flag_l.unwrap_or("error".to_owned()));
        env_logger::init().unwrap();
    }
    trace!("Logger registered");

    let ref mut stderr = io::stderr();
    let infile: Box<Read> = if args.arg_src == "-" {
        if cfg!(target_arch="asmjs") {
            // Running this with node, stdin read an unlimited amount of I-don't-know-what.
            // Zeroes maybe.
            panic!("Reading source code from standard input is not supported in javascript");
        }
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
