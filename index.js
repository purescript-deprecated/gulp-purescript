'use strict';

var gulpPurescript = require('./output/GulpPurescript.Plugin');

function compile(options) {
  return gulpPurescript.compile(options)();
}

function bundle(options) {
  return gulpPurescript.bundle(options)();
}

function docs(options) {
  return gulpPurescript.docs(options)();
}

function psci(options) {
  return gulpPurescript.psci(options)();
}

module.exports.compile = compile;

module.exports.bundle = bundle;

module.exports.docs = docs;

module.exports.psci = psci;
