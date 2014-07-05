'use strict';

var gutil = require('gulp-util')
  , through = require('through2')
  , lodash = require('lodash')
  , cp = require('child_process')
  , PLUGIN = 'gulp-purescript'
  , OPTIONS = {
      psc: {
        flags: {
          noPrelude: '--no-prelude',
          noOpts: '--no-opts',
          noMagicDo: '--no-magic-do',
          noTco: '--no-tco',
          runtimeTypeChecks: '--runtime-type-checks',
          main: '--main',
          verboseErrors: '--verbose-errors'
        }
        , single: {browserNamespace: '--browser-namespace', externs: '--externs', main: '--main', output: '--output'}
        , multi: {modules: '--module', codegen: '--codegen'}
      },
      pscMake: {
        flags: {
          noPrelude: '--no-prelude',
          noOpts: '--no-opts',
          noMagicDo: '--no-magic-do',
          noTco: '--no-tco',
          runtimeTypeChecks: '--runtime-type-checks',
          verboseErrors: '--verbose-errors'
        }
        , single: {browserNamespace: '--browser-namespace', output: '--output'}
        , multi: {}
      },
      docgen: {
        flags: {
          hierarchy: '--hierarchy-images'
        }
        , single: {}
        , multi: {}
      }
    }
;

function options(o, opts) {
  return Object.keys(opts || {}).reduce(function(b, a){
    if (a in o.flags && opts[a] === true) return b.concat([o.flags[a]]);
    else if (a in o.single && typeof opts[a] === 'string') return b.concat([o.single[a] + '=' + opts[a]]);
    else if (a in o.multi) {
      if (typeof opts[a] === 'string') return b.concat([o.multi[a] + '=' + opts[a]]);
      else {
        return b.concat(opts[a].map(function(x){
          return o.multi[a] + '=' + x;
        }));
      }
    }
    else return b;
  }, []);
}

function acc(f) {
  var files = [];
  return through.obj(function(file, env, cb){
    if (file.isNull()) {
      this.push(file);
      return cb();
    }
    if (file.isStream()) {
      this.emit('error', new gutil.PluginError(PLUGIN, 'Streaming not supported'));
      return cb();
    }
    files.push(file.path);
    cb();
  }, function(cb){f.apply(this, [files, cb]);});
}

function psc(opts) {
  var output = opts && opts.output ? opts.output : 'psc.js'
    , opts$prime = lodash.omit(opts || {}, 'output')
  ;
  // The `output` given there will be passed to gulp, not `psc` command.
  // If it was passed to `psc` command, the file will be created and gulp
  // won't receive any input stream from this function.
  return acc(function(files, cb){
    var args = files.concat(options(OPTIONS.psc, opts$prime))
      , cmd = cp.spawn('psc', args)
      , buffero = new Buffer(0)
      , buffere = new Buffer(0)
      , that = this
    ;
    cmd.stdout.on('data', function(stdout){buffero = Buffer.concat([buffero, new Buffer(stdout)]);});
    cmd.stderr.on('data', function(stderr){buffere = Buffer.concat([buffere, new Buffer(stderr)]);});
    cmd.on('close', function(code){
      if (!!code) that.emit('error', new gutil.PluginError(PLUGIN, buffere.toString()));
      else {
        that.push(new gutil.File({
          path: output,
          contents: buffero
        }));
      }
      cb();
    });
  });
}

function pscMake(opts) {
  return acc(function(files, cb){
    var args = options(OPTIONS.pscMake, opts).concat(files)
      , cmd = cp.spawn('psc-make', args)
      , that = this
    ;
    cmd.stdout.on('data', function(stdout){
      gutil.log('Stdout from \'' + gutil.colors.cyan('psc-make') + '\'\n' + gutil.colors.magenta(stdout));
    });
    cmd.stderr.on('data', function(stderr){
      gutil.log('Stderr from \'' + gutil.colors.cyan('psc-make') + '\'\n' + gutil.colors.magenta(stderr));
    });
    cmd.on('close', function(code){
      if (!!code) that.emit('error', new gutil.PluginError(PLUGIN, 'psc-make has failed'));
      cb();
    });
  });
}

function docgen(opts) {
  return acc(function(files, cb){
    var args = options(OPTIONS.docgen, opts).concat(files)
      , cmd = cp.spawn('docgen', args)
      , buffero = new Buffer(0)
      , buffere = new Buffer(0)
      , that = this
    ;
    cmd.stdout.on('data', function(stdout){buffero = Buffer.concat([buffero, new Buffer(stdout)]);});
    cmd.stderr.on('data', function(stderr){buffere = Buffer.concat([buffere, new Buffer(stderr)]);});
    cmd.on('close', function(code){
      if (!!code) that.emit('error', new gutil.PluginError(PLUGIN, buffere.toString()));
      else {
        that.push(new gutil.File({
          path: '.',
          contents: buffero
        }));
      }
      cb();
    });
  });
}

module.exports = {
  docgen: docgen,
  psc: psc,
  pscMake: pscMake
}
