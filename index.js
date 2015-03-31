'use strict';

var fs = require('fs');

var path = require('path');

var child_process = require('child_process');

var gutil = require('gulp-util');

var through2 = require('through2');

var lodash = require('lodash');

var which = require('which');

var minimist = require('minimist');

var logalot = require('logalot');

var multipipe = require('multipipe');

var resolveBin = require('resolve-bin');

var options = require('./options');

var packageJson = require('./package.json');

var pluginName = packageJson.name;

var purescriptPackage = 'purescript';

var psciFilename = '.psci';

var psciLoadCommand = ':m';

var argv = minimist(process.argv.slice(2));

var isVerbose = argv.verbose;

var cwd = process.cwd();

var PluginError = gutil.PluginError;

var File = gutil.File;

function resolve(cmd, callback) {
  var err = 'Failed to find ' + gutil.colors.cyan(cmd) + '. Please ensure it is available on your system.';

  resolveBin(purescriptPackage, {executable: cmd}, function(e, bin){
    if (!e) callback(null, bin);
    else {
      which(cmd, function(e){
        if (e) callback(err);
        else callback(null, cmd);
      });
    }
  });
}

function execute(cmd, args, callback) {
  resolve(cmd, function(e, bin){
    if (e) callback(new PluginError(pluginName, e));
    else callback(null, child_process.spawn(bin, args));
  });
}

function mkOptions(o, opts) {
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

function collectPaths() {
  var paths = [];

  function transform(chunk, encoding, callback) {
    if (chunk.isNull()) callback(null, chunk);
    else if (chunk.isStream()) callback(new PluginError(pluginName, 'Streaming not supported'));
    else {
      paths.push(chunk.path);
      callback();
    }
  }

  function flush(callback){
    this.push(paths);
    callback();
  };

  return through2.obj(transform, flush);
}

function psc(opts) {
  var output = opts && opts.output ? opts.output : 'psc.js';

  var opts$prime = lodash.omit(opts || {}, 'output');

  // The `output` given there will be passed to gulp, not `psc` command.
  // If it was passed to `psc` command, the file will be created and gulp
  // won't receive any input stream from this function.
  function transform(chunk, encoding, callback) {
    var args = chunk.concat(mkOptions(options.psc, opts$prime));

    var buffero = new Buffer(0);

    var buffere = new Buffer(0);

    execute(options.psc.cmd, args, function(e, cmd){
      if (e) callback(e);
      else {
        cmd.stdout.on('data', function(stdout){buffero = Buffer.concat([buffero, new Buffer(stdout)]);});

        cmd.stderr.on('data', function(stderr){buffere = Buffer.concat([buffere, new Buffer(stderr)]);});

        cmd.on('close', function(code){
          if (code !== 0) callback(new PluginError(pluginName, buffere.toString()));
          else {
            callback(null, new File({
              path: output,
              contents: buffero
            }));
          }
        });
      }
    });
  }

  return multipipe(collectPaths(), through2.obj(transform));
}

function pscMake(opts) {
  function transform(chunk, encoding, callback) {
    var args = mkOptions(options.pscMake, opts).concat(chunk);

    var buffero = new Buffer(0);

    var buffere = new Buffer(0);

    execute(options.pscMake.cmd, args, function(e, cmd){
      if (e) callback(e);
      else {
        cmd.stdout.on('data', function(stdout){buffero = Buffer.concat([buffero, new Buffer(stdout)]);});

        cmd.stderr.on('data', function(stderr){buffere = Buffer.concat([buffere, new Buffer(stderr)]);});

        cmd.on('close', function(code){
          var message =
            function() { return [ gutil.colors.cyan(options.pscMake.cmd)
                                , buffero.toString()
                                , buffere.toString() ].join('\n') };

          if (code !== 0) callback(new PluginError(pluginName, message()));
          else {
            if (isVerbose) logalot.info(message());
            callback();
          }
        });
      }
    });
  };

  return multipipe(collectPaths(), through2.obj(transform));
}

function pscDocs(opts) {
  function transform(chunk, encoding, callback) {
    var args = mkOptions(options.pscDocs, opts).concat(chunk);

    var buffero = new Buffer(0);

    var buffere = new Buffer(0);

    execute(options.pscDocs.cmd, args, function(e, cmd){
      if (e) callback(e);
      else {
        cmd.stdout.on('data', function(stdout){buffero = Buffer.concat([buffero, new Buffer(stdout)]);});

        cmd.stderr.on('data', function(stderr){buffere = Buffer.concat([buffere, new Buffer(stderr)]);});

        cmd.on('close', function(code){
          if (code !== 0) callback(new PluginError(pluginName, buffere.toString()));
          else {
            callback(null, new File({
              path: '.',
              contents: buffero
            }));
          }
        });
      }
    });
  }

  return multipipe(collectPaths(), through2.obj(transform));
}

function dotPsci(opts) {
  function transform(chunk, encoding, callback) {
    if (chunk.isNull()) callback(null, chunk);
    else if (chunk.isStream()) callback(new PluginError(pluginName, 'Streaming not supported'));
    else {
      var buffer = new Buffer(psciLoadCommand + ' ' + path.relative(cwd, chunk.path) + '\n');
      callback(null, buffer);
    }
  }

  return multipipe(through2.obj(transform), fs.createWriteStream(psciFilename));
}

module.exports = {
  psc: psc,
  pscMake: pscMake,
  pscDocs: pscDocs,
  dotPsci: dotPsci
}
