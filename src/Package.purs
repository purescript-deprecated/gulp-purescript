module GulpPurescript.Package
  ( Pkg()
  , Package(..)
  , package
  ) where

import Control.Monad.Eff (Eff())

import Data.Either (either)
import Data.Foreign (Foreign())
import Data.Foreign.Class (IsForeign, read, readProp)
import Data.Function
import Data.Maybe (Maybe(..))

foreign import data Pkg :: !

newtype Package = Package { name :: String }

instance isForeignPackage :: IsForeign Package where
  read a = (\a -> Package { name: a }) <$> readProp "name" a

package :: forall eff. Eff (package :: Pkg | eff) (Maybe Package)
package = either (const Nothing) Just <$> read <$> packageFn

foreign import packageFn """
function packageFn() {
  var pkg = require('../../package.json');
  return pkg;
}
""" :: forall eff. Eff (package :: Pkg | eff) Foreign
