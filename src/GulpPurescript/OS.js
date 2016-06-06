'use strict';

var os = require('os');

function platformFn() {
  return os.platform();
}

exports.platformFn = platformFn;
