module GulpPurescript.FS
  ( FS()
  , writeFile
  ) where

import Control.Monad.Aff (Aff(), makeAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (Error())

import Data.Function

foreign import data FS :: !

writeFile :: forall eff. String -> String -> Aff (fs :: FS | eff) Unit
writeFile filename contents = makeAff $ runFn4 writeFileFn filename contents

foreign import writeFileFn """
function writeFileFn(filename, contents, errback, callback) {
  return function(){
    var fs = require('fs');
    fs.writeFile(filename, contents, function(error){
      if (error) errback(new Error(error))();
      else callback()();
    });
  };
}
""" :: forall eff. Fn4 String
                       String
                       (Error -> Eff (fs :: FS | eff) Unit)
                       (Unit -> Eff (fs :: FS | eff) Unit)
                       (Eff (fs :: FS | eff) Unit)
