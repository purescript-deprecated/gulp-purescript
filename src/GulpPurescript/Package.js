'use strict';

// module GulpPurescript.Package

var pkg = require('../../package.json');

function packageFn() {
  return pkg;
}

exports.packageFn = packageFn;
