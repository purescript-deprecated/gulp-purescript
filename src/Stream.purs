module GulpPurescript.Stream
  ( Stream()
  , ReadableStream()
  , mkReadableStreamFromAff
  ) where

import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (Error())

import Data.Function

foreign import data Stream :: !

data ReadableStream out

type RunAff eff a = (Error -> Eff eff Unit) -> (a -> Eff eff Unit) -> Aff eff a -> Eff eff Unit

mkReadableStreamFromAff :: forall eff1 eff2 out. Aff eff1 out -> Eff (stream :: Stream | eff2) (ReadableStream out)
mkReadableStreamFromAff = runFn2 mkReadableStreamFromAffFn runAff

foreign import mkReadableStreamFromAffFn """
function mkReadableStreamFromAffFn(runAff, aff) {
  return function(){
    var stream = require('stream');

    var objectMode = true;

    var readable = new stream.Readable({objectMode: objectMode});

    readable._read = function(){
    };

    function onError(e) {
      return function(){
        readable.emit('error', e);
      };
    }

    function onSuccess(a) {
      return function(){
        readable.push(a);
        readable.push(null);
      };
    }

    var eff = runAff(onError)(onSuccess)(aff);

    eff();

    return readable;
  };
}
""" :: forall eff1 eff2 out. Fn2 (RunAff eff1 out)
                                 (Aff eff1 out)
                                 (Eff (stream :: Stream | eff2) (ReadableStream out))
