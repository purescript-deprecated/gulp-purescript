'use strict';

// module GulpPurescript.Options

var glob = require('glob');

var camelcase = require('camelcase');

function expandGlob() {
  return function(pattern) {
    return glob.sync(pattern);
  };
}

function camelcaseFn(a) {
  return camelcase(a);
}

exports.expandGlob = expandGlob;

exports.camelcaseFn = camelcaseFn;
