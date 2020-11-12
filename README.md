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

This will build (mostly) static binaries under a `target` directory.

### Web

Binaries are provided for both the command-line compiler (`rcc`) and a CGI
script suitable for running the compiler as a web service (`cgi`). For
convenience, the included Makefile will build zip files for the CGI script and
an HTML interface, as well as a pure-Javascript version that does not require
any special server support.

For targeting javascript, you'll need a version of rustc with support for the
`asmjs-unknown-emscripten` target. Currently, that means [building it from
source][rust-users-emscripten]. Ensure that version of
rustc is the one that will run when you invoke `rustc`, or set the `RUSTC`
environment variable to point to it so invoking Cargo will use it. (This
should be easier to set up in the near future!)

[rust-users-emscripten]: https://users.rust-lang.org/t/compiling-to-the-web-with-rust-and-emscripten/7627

Then you should be able to invoke Make to build the pure-Javascript version:

    make dist-purejs.zip

## Binaries

The following binaries are included:

 * `rcc`: command-line compiler. Pass a filename on the command line, and it
   emits assembly to stdout.
 * `cgi`: CGI application. The HTML file(s) in the `web` directory may be used
   as a simple frontend.
