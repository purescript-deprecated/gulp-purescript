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

  var promise = purescript.psc({src: fixture});

  promise.then(function(){
    t.pass('should output a compiled result');
  });
});

test('psc - error', function(t){
  t.plan(2);

  var fixture = 'Fixture2.purs';

  var promise = purescript.psc({src: fixture});

  promise.catch(function(error){
    t.ok(/"where"/.test(error.message), 'should have a failure message');
    t.equal('Error', error.name);
  });
});

test('psc - invalid option type', function(t){
  t.plan(2);

  var promise = purescript.psc({src: 10});

  promise.catch(function(error){
    t.ok(/type mismatch/i.test(error.message), 'should have a failure message');
    t.equal('Error', error.name);
  });
});

test('psc-bundle - basic', function(t){
  t.plan(1);

  var fixture = 'foreign.js';

  var promise = purescript.pscBundle({src: fixture});

  promise.then(function(stream){
    stream.pipe(through2.obj(function(chunk, encoding, callback){
      t.ok(/psc-bundle/.test(chunk.toString()), 'should have a compiled result');
      callback();
    }));
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

