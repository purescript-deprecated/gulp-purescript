'use strict';

var gulpPurescript = require('GulpPurescript.Plugin');

var Promise = require('promise');

function promisify(aff) {
  return new Promise(function(resolve, reject){
    var errback = function(error){
      return function(){
        reject(error);
      };
    };
    var callback = function(result){
      return function(){
        resolve(result);
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

function psci(options) {
  return promisify(gulpPurescript.psci(options));
}

module.exports.psc = psc;

module.exports.pscBundle = pscBundle;

module.exports.pscDocs = pscDocs;

module.exports.psci = psci;
