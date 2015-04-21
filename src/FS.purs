module GulpPurescript.FS
  ( FS()
  , Stream()
  , createWriteStream
  ) where

import Control.Monad.Eff (Eff())

foreign import data FS :: !

data Stream i o

foreign import createWriteStream """
function createWriteStream(path) {
  return function(){
    var fs = require('fs');
    return fs.createWriteStream(path);
  };
}
""" :: forall eff. String -> Eff (fs :: FS | eff) (Stream String Unit)
