module GulpPurescript.Logalot
  ( Logalot
  , info
  ) where

import Prelude (Unit)

import Control.Monad.Eff (Eff)

foreign import data Logalot :: !

foreign import info :: forall eff. String -> Eff (logalot :: Logalot | eff) Unit
