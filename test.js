'use strict';

var gutil = require('gulp-util')
  , assert = require('assert')
  , fs = require('fs')
  , purescript = require('./')
;

it('should compile purescript', function(cb){
  var stream = purescript.psc({noPrelude: true})
    , fixture = 'Fixture.purs.hs'
  ;

  stream.on('data', function(file){
    assert(/Fixture/.test(file.contents.toString()));
    assert.equal('psc.js', file.path);
    cb();
  });

  fs.readFile(fixture, function(e, buffer){
    if (e) cb(assert(false));
    else {
      stream.write(new gutil.File({
        cwd: __dirname,
        base: __dirname,
        path: __dirname + '/' + fixture,
        contents: buffer,
        stat: {mtime: new Date()}
      }));
      stream.end();
    }
  });
});

it('should compile purescript to specified output, without creating file', function(cb){
  var fixture = 'Fixture.purs.hs'
    , output  = 'output.js'
    , stream  = purescript.psc({noPrelude: true, output: output})
  ;

  stream.on('data', function(file){
    assert(!fs.existsSync(__dirname + "/" + output));
    assert.equal(output, file.path);
    cb();
  });

  fs.readFile(fixture, function(e, buffer){
    if (e) cb(assert(false));
    else {
      stream.write(new gutil.File({
        cwd: __dirname,
        base: __dirname,
        path: __dirname + '/' + fixture,
        contents: buffer,
        stat: {mtime: new Date()}
      }));
      stream.end();
    }
  });
});
