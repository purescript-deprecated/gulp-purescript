'use strict';

var gutil = require('gulp-util')
  , through = require('through2')
  , lodash = require('lodash')
  , cp = require('child_process')
  , fs = require('fs')
  , path = require('path')
  , which = require('which')
  , PLUGIN = 'gulp-purescript'
  , DOTPSCI = '.psci'
  , LOADM = ':m'
  , CWD = process.cwd()
  , OPTIONS = {
      psc: {
        cmd: 'psc',
        flags: {
          noPrelude: '--no-prelude',
          noOpts: '--no-opts',
          noMagicDo: '--no-magic-do',
          noTco: '--no-tco',
          main: '--main',
          verboseErrors: '--verbose-errors'
        }
        , single: {browserNamespace: '--browser-namespace', externs: '--externs', main: '--main', output: '--output'}
        , multi: {modules: '--module', codegen: '--codegen'}
      },
      pscMake: {
        cmd: 'psc-make',
        flags: {
          noPrelude: '--no-prelude',
          noOpts: '--no-opts',
          noMagicDo: '--no-magic-do',
          noTco: '--no-tco',
          verboseErrors: '--verbose-errors'
        }
        , single: {browserNamespace: '--browser-namespace', output: '--output'}
        , multi: {}
      },
      pscDocs: {
        cmd: 'psc-docs',
        flags: {
          hierarchy: '--hierarchy-images'
        }
        , single: {}
        , multi: {}
      }
    }
;

function run(cmd, args, k) {
  var err = [ 'Failed to find ' + gutil.colors.magenta(cmd), 'in your path.'
            , 'Please ensure that ' + gutil.colors.magenta(cmd)
            , 'is available on your system.' ].join(' ')
    , that = this
  ;
  which(cmd, function(e){
    if (e) that.emit('error', new gutil.PluginError(PLUGIN, err));
    else k(cp.spawn(cmd, args));
  });
}

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
      , buffero = new Buffer(0)
      , buffere = new Buffer(0)
      , that = this
    ;
    run.apply(this, [OPTIONS.psc.cmd, args, function(cmd){
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
    }]);
  });
}

function pscMake(opts) {
  return acc(function(files, cb){
    var args = options(OPTIONS.pscMake, opts).concat(files)
      , that = this
    ;
    run.apply(this, [OPTIONS.pscMake.cmd, args, function(cmd){
      cmd.stdout.on('data', function(stdout){
        gutil.log('Stdout from \'' + gutil.colors.cyan(OPTIONS.pscMake.cmd) + '\'\n' + gutil.colors.magenta(stdout));
      });
      cmd.stderr.on('data', function(stderr){
        gutil.log('Stderr from \'' + gutil.colors.cyan(OPTIONS.pscMake.cmd) + '\'\n' + gutil.colors.magenta(stderr));
      });
      cmd.on('close', function(code){
        if (!!code) that.emit('error', new gutil.PluginError(PLUGIN, OPTIONS.pscMake.cmd + ' has failed'));
        cb();
      });
    }]);
  });
}

function pscDocs(opts) {
  return acc(function(files, cb){
    var args = options(OPTIONS.pscDocs, opts).concat(files)
      , buffero = new Buffer(0)
      , buffere = new Buffer(0)
      , that = this
    ;
    run.apply(this, [OPTIONS.pscDocs.cmd, args, function(cmd){
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
    }]);
  });
}

function dotPsci(opts) {
  var stream = through.obj(function(file, env, cb){
    if (file.isNull()) {
      this.push(file);
      return cb();
    }
    if (file.isStream()) {
      this.emit('error', new gutil.PluginError(PLUGIN, 'Streaming not supported'));
      return cb();
    }
    this.push(new Buffer(LOADM + ' ' + path.relative(CWD, file.path) + '\n'));
    cb();
  });
  stream.pipe(fs.createWriteStream(DOTPSCI));
  return stream;
}

module.exports = {
  psc: psc,
  pscMake: pscMake,
  pscDocs: pscDocs,
  dotPsci: dotPsci
}
