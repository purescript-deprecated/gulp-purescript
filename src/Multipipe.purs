module GulpPurescript.Multipipe (multipipe2) where

import Data.Function

import GulpPurescript.FS (Stream())

foreign import multipipe2Fn """
function multipipe2Fn(stream1, stream2) {
  var multipipe = require('multipipe');
  return multipipe(stream1, stream2);
}
""" :: forall a b c. Fn2 (Stream a b)
                         (Stream b c)
                         (Stream a c)

multipipe2 :: forall a b c. Stream a b -> Stream b c -> Stream a c
multipipe2 a b = runFn2 multipipe2Fn a b
