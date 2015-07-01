module GulpPurescript.Stream
  ( Stream()
  , ReadableStream()
  , mkReadableStreamFromBuffer
  ) where

import Control.Monad.Eff (Eff())

import GulpPurescript.Buffer (Buffer())

foreign import data Stream :: !

data ReadableStream o

foreign import mkReadableStreamFromBuffer """
function mkReadableStreamFromBuffer(buffer) {
  return function(){
    var Readable = require('stream').Readable;

    var stream = Readable();

    stream._read = function(){
      stream.push(buffer);
      return stream.push(null);
    };

    return stream;
  };
}
""" :: forall eff. Buffer -> Eff (stream :: Stream | eff) (ReadableStream Buffer)
