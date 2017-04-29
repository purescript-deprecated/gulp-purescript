module GulpPurescript.Which
  ( Which
  , which
  ) where

import Prelude (Unit, ($))

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Exception (Error)

import Data.Function.Uncurried (Fn3, runFn3)

foreign import data Which :: Effect

which :: forall eff. String -> Aff (which :: Which | eff) String
which cmd = makeAff $ runFn3 whichFn cmd

foreign import whichFn :: forall eff. Fn3 String
                                          (Error -> Eff (which :: Which | eff) Unit)
                                          (String -> Eff (which :: Which | eff) Unit)
                                          (Eff (which :: Which | eff) Unit)
