module GulpPurescript.Buffer
  ( Buffer()
  , mkBufferFromString
  ) where

data Buffer

foreign import mkBufferFromString :: String -> Buffer
