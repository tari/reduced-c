extern crate parser_combinators;

pub mod syntax;

fn main() {
    println!("{:?}", syntax::Ast::parse("void main(int foo, int bar) { }"));
}
