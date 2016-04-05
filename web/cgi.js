function compileSources(code, callback) {
    var req = new XMLHttpRequest();
    req.onload = function(e) {
        if (req.status == 200) { 
            callback(req.responseText);
        } else {
            callback("Server responded with failure: " + req.status + " " + req.statusText);
        }
    };
    req.onerror = function(e) {
        callback("Error communicating with the server");
    };
    req.onabort = req.onerror;

    req.open('POST', 'cgi-bin/rcc.cgi');
    // Page encoding is utf-8 so this is correct.
    req.setRequestHeader('Content-Type', 'text/plain; charset=UTF-8');
    req.send(code);
}
