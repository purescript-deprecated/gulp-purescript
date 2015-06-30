'use strict';

var fs = require('fs');

var path = require('path');

var test = require('tape');

var gulp = require('gulp');

var through2 = require('through2');

var rewire = require('rewire');

var purescript = require('./');

test('psc - basic', function(t){
  t.plan(2);

  var stream = purescript.psc({noPrelude: true});

  var fixture = 'Fixture1.purs';

  gulp.src(fixture).pipe(stream).
                    pipe(through2.obj(function(chunk, encoding, callback){
    t.ok(/Fixture/.test(chunk.contents.toString()), 'should have a compiled result');
    t.equal('psc.js', chunk.path);
    callback();
  }));
});

test('psc - output option', function(t){
  t.plan(2);

  var fixture = 'Fixture1.purs';

  var output  = 'output.js';

  var stream  = purescript.psc({noPrelude: true, output: output});

  gulp.src(fixture).pipe(stream).
                    pipe(through2.obj(function(chunk, encoding, callback){
    t.ok(!fs.existsSync(__dirname + '/' + output), 'output file should not exist');
    t.equal(output, chunk.path);
    callback();
  }));
});

test('psc - failure', function(t){
  t.plan(2);

  var stream = purescript.psc({noPrelude: true});

  var fixture = 'Fixture2.purs';

  gulp.src(fixture).pipe(stream).
                    on('error', function(e){
    t.ok(/"where"/.test(e.message), 'should have a failure message');
    t.equal('Error', e.name);
  });
});

test('psc - invalid option type', function(t){
  t.plan(2);

  var fixture = 'Fixture1.purs';

  var moduleName = path.basename(fixture, '.purs');

  var stream  = purescript.psc({noPrelude: true, module: moduleName});

  gulp.src(fixture).pipe(stream).
                    on('error', function(e){
    t.ok(/type mismatch/i.test(e.message), 'should have a failure message');
    t.equal('Error', e.name);
  });
});

test('psci - basic', function(t){
  t.plan(1);

  var fixture = 'Fixture1.purs';

  var output = ':m ' + fixture;

  var promise = purescript.psci({src: fixture});

  promise.then(function(){
    fs.readFile('.psci', function(error, result){
      if (error) t.fail(error);
      else t.equal(result.toString(), output);
    });
  });
});

test('psc-make - basic', function(t){
  t.plan(1);

  var purescript = rewire('./');

  var mock = {
    success: function(){
      t.fail('Should not get a log message');
    }
  };

  purescript.__set__('logalot', mock);

  var stream = purescript.pscMake({noPrelude: true});

  var fixture = 'Fixture1.purs';

  gulp.src(fixture).pipe(stream).
                    on('finish', function(){
    t.pass('should output a compiled result');
  });
});

test('psc-make - error', function(t){
  t.plan(2);

  var stream = purescript.pscMake({noPrelude: true});

  var fixture = 'Fixture2.purs';

  gulp.src(fixture).pipe(stream).
                    on('error', function(e){
    t.ok(/"where"/.test(e.message), 'should have a failure message');
    t.equal('Error', e.name);
  });
});

test('psc-make - invalid option type', function(t){
  t.plan(2);

  var stream = purescript.pscMake({noPrelude: 'invalid'});

  var fixture = 'Fixture1.purs';

  gulp.src(fixture).pipe(stream).
                    on('error', function(e){
    t.ok(/type mismatch/i.test(e.message), 'should have a failure message');
    t.equal('Error', e.name);
  });
});
