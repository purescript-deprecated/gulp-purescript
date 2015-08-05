module GulpPurescript.Minimist (minimist) where

import Prelude

import Data.Either (either)
import Data.Foreign (Foreign())
import Data.Foreign.Class (IsForeign, read)
import Data.Function
import Data.Maybe (Maybe(..))

minimist :: forall a. (IsForeign a) => Array String -> (Maybe a)
minimist argv = either (const Nothing) Just (read $ minimistFn argv)

foreign import minimistFn :: Array String -> Foreign
