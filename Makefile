# Extra parameters to emcc for final compilation of javascript.
# Perhaps '--closure 1' or '-s ASSERTIONS=1'
EMCC_EXTRA_LINK ?= 

## Generic targets, etc
.PHONY: all clean clean-web
all: dist-purejs.zip dist-cgi.zip
clean: clean-web
	cargo clean
clean-web:
	rm -f web/cgi-bin/rcc.cgi web/{purejs,cgi}.html web/rcc.js{,.mem} *.zip

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
		-C link-args='-s ALLOW_MEMORY_GROWTH=1 -O3 $(EMCC_EXTRA_LINK)'

web/purejs.html: web/template.html web/purejs.js tsubst.awk
	awk -f tsubst.awk -v templatefile=web/purejs.js web/template.html > web/purejs.html

web/% : $(CARGO_JS_DIR)/%
	cp $^ web

## CGI version
dist-cgi.zip: web/cgi.html web/cgi-bin/rcc.cgi web/throbber.svg
	zip dist-cgi.zip $^

web/cgi.html: web/template.html web/cgi.js tsubst.awk
	awk -f tsubst.awk -v templatefile=web/cgi.js web/template.html > web/cgi.html

target/release/cgi:
	cargo build --release

web/cgi-bin/rcc.cgi: target/release/cgi
	mkdir -p web/cgi-bin
	cp $^ $@

