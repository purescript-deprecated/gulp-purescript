module GulpPurescript.Through2
  ( Through2()
  , RunAff()
  , objStream
  , accStream
  ) where

import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (Error())

import Data.Function

import GulpPurescript.FS (Stream())

foreign import data Through2 :: !

type RunAff eff a = (Error -> Eff eff Unit) -> (a -> Eff eff Unit) -> Aff eff a -> Eff eff Unit

objStream :: forall eff1 eff2 input output. (input -> Aff eff1 output) -> Eff (through2 :: Through2 | eff2) (Stream input output)
objStream = runFn2 objStreamFn runAff

foreign import objStreamFn """
  function objStreamFn(runAff, aff) {
    return function(){
      var through2 = require('through2');

      function transform(chunk, encoding, callback) {
        function onError(e) {
          return function(){
            callback(e);
          };
        }

        function onSuccess(a) {
          return function(){
            callback(null, a);
          };
        }

        var aff$prime = aff(chunk);

        var eff = runAff(onError)(onSuccess)(aff$prime);

        return eff();
      }

      return through2.obj(transform);
    };
  }
""" :: forall eff1 eff2 input output. Fn2 (RunAff eff1 output)
                                          (input -> Aff eff1 output)
                                          (Eff (through2 :: Through2 | eff2) (Stream input output))

accStream :: forall eff1 eff2 input output. (input -> Aff eff1 output) -> Eff (through2 :: Through2 | eff2) (Stream input [output])
accStream = runFn2 accStreamFn runAff

foreign import accStreamFn """
  function accStreamFn(runAff, aff) {
    return function(){
      var through2 = require('through2');

      var arr = [];

      function transform(chunk, encoding, callback) {
        function onError(e) {
          return function(){
            callback(e);
          };
        }

        function onSuccess(a) {
          return function(){
            arr.push(a);
            callback();
          };
        }

        var aff$prime = aff(chunk);

        var eff = runAff(onError)(onSuccess)(aff$prime);

        return eff();
      }

      function flush(callback) {
        this.push(arr);
        callback();
      }

      return through2.obj(transform, flush);
    };
  }
""" :: forall eff1 eff2 input output. Fn2 (RunAff eff1 output)
                                          (input -> Aff eff1 output)
                                          (Eff (through2 :: Through2 | eff2) (Stream input [output]))
