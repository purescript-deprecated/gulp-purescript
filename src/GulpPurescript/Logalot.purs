module GulpPurescript.Logalot
  ( Logalot
  , info
  ) where

import Prelude (Unit)

import Control.Monad.Eff (Eff, kind Effect)

foreign import data Logalot :: Effect

foreign import info :: forall eff. String -> Eff (logalot :: Logalot | eff) Unit
