module GulpPurescript.Buffer
  ( Buffer()
  , mkBufferFromString
  ) where

data Buffer

foreign import mkBufferFromString """
function mkBufferFromString(str) {
  return new Buffer(str);
}
""" :: String -> Buffer
