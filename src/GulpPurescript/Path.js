'use strict';

var path = require('path');

function relativeFn(from, to) {
  return path.relative(from, to);
}

exports.relativeFn = relativeFn;
