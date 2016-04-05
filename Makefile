
.PHONY: purejs
purejs: web/purejs.html web/rcc.js web/worker.js

JS_TARGETS := rcc.js
CARGO_JS_DIR := target/asmjs-unknown-emscripten/debug
CARGO_JS_TARGETS := $(addprefix $(CARGO_DIR)/,$(JS_TARGETS))

# We build in debug mode because emscripten currently aborts on some of the
# code emitted by rustc in release mode.
$(CARGO_JS_TARGETS):
	cargo rustc --target=asmjs-unknown-emscripten --bin rcc -- \
		-C link-args='-s ALLOW_MEMORY_GROWTH=1'

web/% : $(CARGO_JS_DIR)/%
	cp $^ web
