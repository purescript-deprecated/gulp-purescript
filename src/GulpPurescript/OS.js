'use strict';

// module GulpPurescript.OS

var os = require('os');

function platformFn() {
  return os.platform();
}

exports.platformFn = platformFn;
