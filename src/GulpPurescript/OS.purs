module GulpPurescript.OS
  ( OS()
  , Platform(..)
  , platform
  ) where

import Prelude

import Control.Monad.Eff (Eff())

import Data.Either (either)
import Data.Foreign (Foreign())
import Data.Foreign.Class (IsForeign, read)
import Data.Maybe (Maybe(..))

foreign import data OS :: !

data Platform = Darwin | Linux | Win32

instance showPlatform :: Show Platform where
  show a = case a of
                Darwin -> "darwin"
                Linux -> "linux"
                Win32 -> "win32"

instance isForeignPlatform :: IsForeign Platform where
  read a = (\a -> case a of
                       "darwin" -> Darwin
                       "linux" -> Linux
                       "win32" -> Win32) <$> read a

platform :: forall eff. Eff (os :: OS | eff) (Maybe Platform)
platform = either (const Nothing) Just <$> read <$> platformFn

foreign import platformFn :: forall eff. Eff (os :: OS | eff) Foreign
