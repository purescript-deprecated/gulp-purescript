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
