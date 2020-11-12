extern crate reduced_c as compiler;

use std::io::Read;
use std::{env, io};

type RequestError = &'static str;

fn main() {
    let content_length = match validate_request() {
        Ok(len) => len,
        Err(e) => {
            print!("Status: 400 Bad Request\r\n\r\n");
            print!("{}", e);
            return;
        }
    };

    println!("Content-Type: text/plain");

    let stdin = io::stdin();

    match compiler::compile(stdin.take(content_length)) {
        Ok(p) => {
            print!("Status: 200 OK\r\n\r\n");
            let _ = compiler::print_assembly(&p, io::stdout());
        }
        Err(e) => {
            print!("Status: 400 Bad Request\r\n\r\n");
            print!("{}", e);
        }
    }
}

const MAX_REQUEST: u64 = 65535;

fn validate_request() -> Result<u64, RequestError> {
    match env::var("CONTENT_TYPE") {
        Err(_) => return Err("Content-Type must be specified"),
        Ok(s) => {
            if s != "text/plain; charset=UTF-8" {
                return Err("Content-Type must be text/plain, UTF-8");
            }
        }
    }

    match env::var("CONTENT_LENGTH") {
        Err(_) => Err("Content-Length must be specified"),
        Ok(s) => match str::parse::<u64>(&s) {
            Err(_) => Err("Content-Length must be a non-negative integer"),
            Ok(len) if len > MAX_REQUEST => Err("Content-Length too large"),
            Ok(len) => Ok(len),
        },
    }
}
