module GulpPurescript.Logalot
  ( Logalot()
  , info
  ) where

import Control.Monad.Eff (Eff())

foreign import data Logalot :: !

foreign import info """
function info(message) {
  return function(){
    var logalot = require('logalot');
    logalot.info(message);
  };
}
""" :: forall eff. String -> Eff (logalot :: Logalot | eff) Unit
