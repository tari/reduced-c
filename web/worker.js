var output = '';
// Two conditions must be satisfied before we can callMain():
//  * We have received some source code
//  * The runtime and all of its dependencies have loaded
var input = null;
var runtimeReady = false;

function print(s) {
    if (s == 'Calling stub instead of signal()' || !runtimeReady) {
        // Emscripten prints that message in response to what I assume is
        // rust runtime startup, and warnings (in particular, "run() called
        // but dependencies remain") before onRuntimeInitialized triggers
        // are beyond our control. Log but do not report to the user.
        console.log(s);
    } else {
        output += s + '\n';
    }
}

var Module = {
    noInitialRun: true,
    print: print,
    printErr: print,
    onRuntimeInitialized: function() {
        runtimeReady = true;
        maybeRunCompile();
    }
};

importScripts('rcc.js');

function maybeRunCompile() {
    if (input !== null && runtimeReady) {
        try {
            FS.writeFile('input.rcc', input);
            Module.callMain(['input.rcc']);
            postMessage(output);
        } catch (e) {
            console.error(e);
            postMessage("Unexpected error running compiler:\n" + e.toString());
        }
    } else {
        if (input === null) {
            console.log('Runtime ready but no source code received yet');
        } else {
            console.log('Source code received but runtime not yet ready');
        }
    }
}

onmessage = function(e) {
    input = e.data;
    maybeRunCompile();
}
