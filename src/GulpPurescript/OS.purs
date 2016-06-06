module GulpPurescript.OS
  ( OS
  , Platform(..)
  , platform
  ) where

import Prelude (class Show, (<$>), (<>), const)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)

import Data.Either (either)
import Data.Foreign (Foreign)
import Data.Foreign.Class (class IsForeign, read)
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
                       "win32" -> Win32
                       _ -> unsafeThrow ("Unhandled platform: " <> a)) <$> read a

platform :: forall eff. Eff (os :: OS | eff) (Maybe Platform)
platform = either (const Nothing) Just <$> read <$> platformFn

foreign import platformFn :: forall eff. Eff (os :: OS | eff) Foreign
