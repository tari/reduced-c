extern crate docopt;
#[macro_use]
extern crate log;

extern crate reduced_c as compiler;

use docopt::Docopt;
use std::fs::File;
use std::io::{self, BufReader, Read, Write};
use std::process;

mod logger {
    use ::log::{Level, LevelFilter, Metadata, Record, SetLoggerError};
    struct StdoutLogger(Level);

    impl ::log::Log for StdoutLogger {
        fn enabled(&self, metadata: &Metadata) -> bool {
            metadata.level() <= self.0
        }

        fn log(&self, record: &Record) {
            if self.enabled(record.metadata()) {
                println!("{} - {}", record.level(), record.args());
            }
        }

        fn flush(&self) {}
    }

    pub fn init(ll: LevelFilter) -> Result<(), SetLoggerError> {
        ::log::set_max_level(ll);
        ::log::set_boxed_logger(Box::new(StdoutLogger(ll.to_level().unwrap())))
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

pub fn main() {
    let args = Docopt::new(USAGE)
        .and_then(|d| {
            d.version(Some(env!("CARGO_PKG_VERSION").to_owned()))
                .parse()
        })
        .unwrap_or_else(|e| e.exit());
    debug!("{:?}", args);

    if cfg!(target_os = "emscripten") {
        // Logging in javascript is a mess, so we'll just spew to stdout.
        // (Environment variables don't exist, for one.)
        use std::str::FromStr;
        let ll = args.find("-l").map_or(log::LevelFilter::Error, |ref v| {
            FromStr::from_str(v.as_str()).expect("Invalid log level")
        });
        logger::init(ll).unwrap();
    } else {
        // If not javascript, we can use env vars. Note -l LEVEL will override
        // any existing setting.
        std::env::set_var("RUST_LOG", args.get_str("-l"));
        env_logger::init();
    }
    trace!("Logger registered");

    let ref mut stderr = io::stderr();
    let src_filename = args.get_str("<src>");
    let infile: Box<dyn Read> = if src_filename == "-" {
        if cfg!(target_os = "emscripten") {
            panic!("Reading source code from standard input is not supported in javascript");
        }
        Box::new(io::stdin())
    } else {
        match File::open(src_filename) {
            Ok(f) => Box::new(f),
            Err(e) => {
                write!(stderr, "Failed to read {}: {}\n", src_filename, e).unwrap();
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
