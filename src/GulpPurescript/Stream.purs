module GulpPurescript.Stream
  ( Stream
  , ReadableStream
  , mkReadableStreamFromAff
  ) where

import Prelude

import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Exception (Error)

import Data.Function.Uncurried (Fn2, runFn2)

foreign import data Stream :: Effect

data ReadableStream out

type RunAff eff a = (Error -> Eff eff Unit) -> (a -> Eff eff Unit) -> Aff eff a -> Eff eff Unit

mkReadableStreamFromAff :: forall eff1 eff2 out. Aff eff1 out -> Eff (stream :: Stream | eff2) (ReadableStream out)
mkReadableStreamFromAff = runFn2 mkReadableStreamFromAffFn \x y z -> void (runAff x y z)

foreign import mkReadableStreamFromAffFn :: forall eff1 eff2 out. Fn2 (RunAff eff1 out)
                                                                      (Aff eff1 out)
                                                                      (Eff (stream :: Stream | eff2) (ReadableStream out))
