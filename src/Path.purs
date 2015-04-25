module GulpPurescript.Path (relative) where

import Data.Function

relative :: String -> String -> String
relative from to = runFn2 relativeFn from to

foreign import relativeFn """
function relativeFn(from, to) {
  var path = require('path');
  return path.relative(from, to);
}
""" :: Fn2 String String String
