'use strict';

// module GulpPurescript.Glob

var glob = require('glob');

var async = require('async');

function globFn(pattern, errback, callback) {
  return function(){
    glob(pattern, function(error, result){
      if (error) errback(new Error(error))();
      else callback(result)();
    });
  };
}

function globAllFn(patterns, errback, callback) {
  return function(){
    async.map(patterns, glob, function(error, result){
      if (error) errback(new Error(error))();
      else callback(result)();
    });
  };
}

exports.globFn = globFn;

exports.globAllFn = globAllFn;
