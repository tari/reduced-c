# Extra parameters to emcc for final compilation of javascript.
# Perhaps '--closure 1'
EMCC_EXTRA_LINK ?= 

## Generic targets, etc
.PHONY: all clean clean-web
all: dist-purejs.zip dist-cgi.zip
clean: clean-web
	cargo clean
clean-web:
	rm -f web/cgi-bin/rcc.cgi web/rcc.js *.zip

## Javascript-only version
JS_TARGETS := rcc.js rcc.js.mem
CARGO_JS_DIR := target/asmjs-unknown-emscripten/debug
CARGO_JS_TARGETS := $(addprefix $(CARGO_JS_DIR)/,$(JS_TARGETS))

dist-purejs.zip: web/purejs.html web/worker.js web/throbber.svg $(addprefix web/,$(JS_TARGETS))
	zip dist-purejs.zip $^

# We build in debug mode because emscripten currently aborts on some of the
# code emitted by rustc in release mode.
$(CARGO_JS_TARGETS):
	cargo rustc --target=asmjs-unknown-emscripten --bin rcc -- \
		-C link-args='-s ALLOW_MEMORY_GROWTH=1 -O2 $(EMCC_EXTRA_LINK)'

web/% : $(CARGO_JS_DIR)/%
	cp $^ web

## CGI version
dist-cgi.zip: web/cgi.html web/cgi-bin/rcc.cgi
	zip dist-cgi.zip $^

target/release/cgi:
	cargo build --release

web/cgi-bin/rcc.cgi: target/release/cgi
	mkdir -p web/cgi-bin
	cp $^ $@

