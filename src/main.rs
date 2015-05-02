extern crate parser_combinators;

pub mod syntax;

fn main() {
    println!("{:?}", syntax::parse_str("void main(int foo) {if(1){if (0) { foo = 0; } else { foo = 1; }}}"));
}
