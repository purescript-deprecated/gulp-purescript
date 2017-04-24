module GulpPurescript.OS
  ( OS
  , Platform(..)
  , platform
  ) where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Control.Monad.Except (runExcept)

import Data.Either (either)
import Data.Foreign (F, Foreign, readString)
import Data.Maybe (Maybe(..))

foreign import data OS :: Effect

data Platform = Darwin | Linux | Win32

instance showPlatform :: Show Platform where
  show = case _ of
    Darwin -> "darwin"
    Linux -> "linux"
    Win32 -> "win32"

readPlatform :: Foreign -> F Platform
readPlatform =
  readString >=> case _ of
    "darwin" -> pure Darwin
    "linux" -> pure Linux
    "win32" -> pure Win32
    a -> unsafeThrow ("Unhandled platform: " <> a)

platform :: forall eff. Eff (os :: OS | eff) (Maybe Platform)
platform = either (const Nothing) Just <$> runExcept <$> readPlatform <$> platformFn

foreign import platformFn :: forall eff. Eff (os :: OS | eff) Foreign
