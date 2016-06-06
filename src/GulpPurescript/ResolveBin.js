'use strict';

var resolveBin = require('resolve-bin');

function resolveBinFn(pkg, options, errback, callback) {
  return function(){
    resolveBin(pkg, options, function(e, bin){
      if (e) errback(e)();
      else callback(bin)();
    })
  };
}

exports.resolveBinFn = resolveBinFn;
