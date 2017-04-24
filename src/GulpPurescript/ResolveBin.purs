module GulpPurescript.ResolveBin
  ( ResolveBin
  , Options(..)
  , resolveBin
  ) where

import Prelude (Unit, ($))

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Exception (Error)

import Data.Function.Uncurried (Fn4, runFn4)

foreign import data ResolveBin :: Effect

type Options = { executable :: String }

resolveBin :: forall eff. String -> Options -> Aff (resolveBin :: ResolveBin | eff) String
resolveBin pkg opts = makeAff $ runFn4 resolveBinFn pkg opts

foreign import resolveBinFn :: forall eff. Fn4 String
                                               Options
                                               (Error -> Eff (resolveBin :: ResolveBin | eff) Unit)
                                               (String -> Eff (resolveBin :: ResolveBin | eff) Unit)
                                               (Eff (resolveBin :: ResolveBin | eff) Unit)
