'use strict';

var gulpPurescript = require('GulpPurescript.Plugin');

function psc(options) {
  var result = gulpPurescript.psc(options);
  return result();
}

function pscMake(options) {
  var result = gulpPurescript.pscMake(options);
  return result();
}

function pscDocs(options) {
  var result = gulpPurescript.pscDocs(options);
  return result();
}

function dotPsci() {
  var result = gulpPurescript.dotPsci();
  return result;
}

module.exports.psc = psc;

module.exports.pscMake = pscMake;

module.exports.pscDocs = pscDocs;

module.exports.dotPsci = dotPsci;
