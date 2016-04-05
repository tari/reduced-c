function compileSources(code, callback) {
    // We need a new emscripten module for every run of the
    // compiler since calling main multiple times on a single
    // Module fails, regardless of whether we set NO_EXIT_RUNTIME.
    var worker = new Worker("worker.js");
    worker.onmessage = function(e) {
        worker.terminate();
        callback(e.data);
    };
    worker.postMessage(code);
}
