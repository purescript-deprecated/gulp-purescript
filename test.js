'use strict';

var fs = require('fs');

var path = require('path');

var test = require('tape');

var gulp = require('gulp');

var through2 = require('through2');

var rewire = require('rewire');

var purescript = require('./');

test('psc - basic', function(t){
  t.plan(1);

  var purescript = rewire('./');

  var mock = {
    success: function(){
      t.fail('Should not get a log message');
    }
  };

  purescript.__set__('logalot', mock);

  var fixture = 'Fixture1.purs';

  var stream = purescript.psc({src: fixture});

  stream.pipe(through2.obj(function(chunk, encoding, callback){
    t.pass('should output a compiled result');
    callback();
  }));
});

test('psc - error', function(t){
  t.plan(2);

  var fixture = 'Fixture2.purs';

  var stream = purescript.psc({src: fixture});

  stream.on('error', function(e){
    t.ok(/"where"/.test(e.message), 'should have a failure message');
    t.equal('Error', e.name);
  });
});

test('psc - invalid option type', function(t){
  t.plan(2);

  try {
    var stream = purescript.psc({src: 10});

    stream.on('error', function(e){
      t.ok(/type mismatch/i.test(error.message), 'should have a failure message');
      t.equal('Error', error.name);
    });
  }
  catch (error) {
    t.ok(/type mismatch/i.test(error.message), 'should have a failure message');
    t.equal('Error', error.name);
  }
});

test('psc-bundle - basic', function(t){
  t.plan(1);

  var fixture = 'foreign.js';

  var stream = purescript.pscBundle({src: fixture});

  stream.pipe(through2.obj(function(chunk, encoding, callback){
    t.ok(/psc-bundle/.test(chunk.contents.toString()), 'should have a compiled result');
    callback();
  }));
});

test('psci - basic', function(t){
  t.plan(2);

  var fixture = 'Fixture1.purs';

  var output = ':m ' + fixture;

  var stream = purescript.psci({src: fixture});

  stream.pipe(through2.obj(function(chunk, encoding, callback){
    t.equal('.psci', chunk.path);
    t.equal(output, chunk.contents.toString());
    callback();
  }));
});
