module GulpPurescript.Minimist (minimist) where

import Data.Either (either)
import Data.Foreign (Foreign())
import Data.Foreign.Class (IsForeign, read)
import Data.Function
import Data.Maybe (Maybe(..))

minimist :: forall a. (IsForeign a) => [String] -> (Maybe a)
minimist argv = either (const Nothing) Just (read $ minimistFn argv)

foreign import minimistFn """
function minimistFn(argv) {
  var minimist = require('minimist');
  return minimist(argv);
}
""" :: [String] -> Foreign
