'use strict';

var which = require('which');

function whichFn(command, errback, callback) {
  return function(){
    which(command, function(e, path){
      if (e) errback(e)();
      else callback(path)();
    })
  };
}

exports.whichFn = whichFn;
