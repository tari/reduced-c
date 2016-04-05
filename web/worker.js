var output = '';
function print(s) {
    if (s == 'Calling stub instead of signal()') {
        // Emscripten prints this message presumably in response to something
        // the Rust runtime does during startup. Ignore it.
        return;
    }
    output += s + '\n';
}
var Module = {
    noInitialRun: true,
    print: print,
    printErr: print
};
importScripts('rcc.js');

onmessage = function(e) {
    var sources = e.data;

    FS.writeFile('input.rcc', sources);
    Module.callMain(['input.rcc']);
    postMessage(output);
}
