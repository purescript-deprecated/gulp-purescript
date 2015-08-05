'use strict';

// module GulpPurescript.GulpUtil

var gutil = require('gulp-util');

function mkPluginErrorFn(name, message) {
  return new gutil.PluginError(name, message);
}

function mkFileFn(path, contents) {
  return new gutil.File({path: path, contents: contents});
}

exports.mkPluginErrorFn = mkPluginErrorFn;

exports.mkFileFn = mkFileFn;
