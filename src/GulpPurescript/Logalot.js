'use strict';

// module GulpPurescript.Logalot

var logalot = require('logalot');

function info(message) {
  return function(){
    logalot.info(message);
  };
}

exports.info = info;
