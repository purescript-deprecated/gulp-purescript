module GulpPurescript.Which
  ( Which()
  , which
  ) where

import Control.Monad.Aff (Aff(), makeAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (Error())

import Data.Function

foreign import data Which :: !

which :: forall eff. String -> Aff (which :: Which | eff) String
which cmd = makeAff $ runFn3 whichFn cmd

foreign import whichFn """
function whichFn(command, errback, callback) {
  return function(){
    var which = require('which');

    which(command, function(e, path){
      if (e) errback(e)();
      else callback(path)();
    })
  };
}
""" :: forall eff. Fn3 String
                       (Error -> Eff (which :: Which | eff) Unit)
                       (String -> Eff (which :: Which | eff) Unit)
                       (Eff (which :: Which | eff) Unit)
