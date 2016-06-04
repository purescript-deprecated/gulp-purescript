'use strict';

var child_process = require('cross-spawn');

function spawnFn(command, args, errback, callback) {
  return function(){
    var process = child_process.spawn(command, args);

    var stdout = new Buffer(0);

    var stderr = new Buffer(0);

    process.stdout.on('data', function(data){
      stdout = Buffer.concat([stdout, new Buffer(data)]);
    });

    process.stderr.on('data', function(data){
      stderr = Buffer.concat([stderr, new Buffer(data)]);
    });

    process.on('close', function(code){
      var result = Buffer.concat([stdout, stderr]).toString();

      if (code !== 0) errback(new Error(result))();
      else callback(result)();
    });
  };
}

exports.spawnFn = spawnFn;
