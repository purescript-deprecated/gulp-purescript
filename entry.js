'use strict';

var gulpPurescript = require('GulpPurescript.Plugin');
var Promise = require('promise');

function promisify(aff) {
  return new Promise(function (resolve, reject) {
    var errback = function (err) {
      return function () {
        reject(err);
      };
    };
    var callback = function (x) {
      return function () {
        resolve(x);
      };
    };
    aff(errback)(callback)();
  });
}

function psc(options) {
  return promisify(gulpPurescript.psc(options));
}

function pscBundle(options) {
  return promisify(gulpPurescript.pscBundle(options));
}

function pscDocs(options) {
  return promisify(gulpPurescript.pscDocs(options));
}

function dotPsci() {
  var result = gulpPurescript.dotPsci();
  return result;
}

module.exports.psc = psc;

module.exports.pscBundle = pscBundle;

module.exports.pscDocs = pscDocs;

module.exports.dotPsci = dotPsci;
