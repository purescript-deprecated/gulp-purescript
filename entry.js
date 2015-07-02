'use strict';

var gulpPurescript = require('GulpPurescript.Plugin');

function psc(options) {
  return gulpPurescript.psc(options)();
}

function pscBundle(options) {
  return gulpPurescript.pscBundle(options)();
}

function pscDocs(options) {
  return gulpPurescript.pscDocs(options)();
}

function psci(options) {
  return gulpPurescript.psci(options)();
}

module.exports.psc = psc;

module.exports.pscBundle = pscBundle;

module.exports.pscDocs = pscDocs;

module.exports.psci = psci;
