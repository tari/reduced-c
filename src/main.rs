extern crate parser_combinators;

pub mod syntax;

fn main() {
    println!("{:?}", syntax::parse_str("void main(int foo) {return 0;}"));
}
