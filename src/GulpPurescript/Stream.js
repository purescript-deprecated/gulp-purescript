'use strict';

// module GulpPurescript.Stream

var stream = require('stream');

function mkReadableStreamFromAffFn(runAff, aff) {
  return function(){
    var objectMode = true;

    var readable = new stream.Readable({objectMode: objectMode});

    readable._read = function(){
    };

    function onError(e) {
      return function(){
        readable.emit('error', e);
      };
    }

    function onSuccess(a) {
      return function(){
        readable.push(a);
        readable.push(null);
      };
    }

    var eff = runAff(onError)(onSuccess)(aff);

    eff();

    return readable;
  };
}

exports.mkReadableStreamFromAffFn = mkReadableStreamFromAffFn;
