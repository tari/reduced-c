# RISC-G reduced-C compiler

Compiler and tools for Reduced-C on RISC-G. See [ASSEMBLY.md](ASSEMBLY.md) for
the assembly language specification, and [LANGUAGE.md](LANGUAGE.md) for the
Reduced-C language specification. A (possibly wrong) grammar for the language
can be found in [grammar.txt](grammar.txt).

## Building

The tools are written in [Rust](https://rust-lang.org), so you'll need `rustc`
and `cargo`. [Rustle](https://github.com/brson/rustle) may be useful if you
don't want to deal with installing the toolchain just to build some binaries.

Once you have everything, just invoke cargo:

    $ cargo build
    # For optimizations (slower to compile)
    $ cargo build --release

This will build (mostly) static binaries under a `target` directory. Due to some
inefficiencies in `rustc`, `reduced-c-syntax` can take a long time to compile-
about four minutes on my reasonably-fast laptop.

## Binaries

The following binaries are included:

 * `rcc`: command-line compiler. Pass a filename on the command line, and it
   emits assembly to stdout.
 * `cgi`: CGI application. The HTML file(s) in the `web` directory may be used
   as a simple frontend.
