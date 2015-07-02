module GulpPurescript.GulpUtil
  ( File()
  , mkPluginError
  , mkFile
  ) where

import Control.Monad.Eff.Exception (Error())

import Data.Function

import GulpPurescript.Buffer (Buffer())

data File

mkPluginError :: String -> String -> Error
mkPluginError name msg = runFn2 mkPluginErrorFn name msg

foreign import mkPluginErrorFn """
function mkPluginErrorFn(name, message) {
  var gutil = require('gulp-util');
  return new gutil.PluginError(name, message);
}
""" :: Fn2 String String Error

mkFile :: String -> Buffer -> File
mkFile path contents = runFn2 mkFileFn path contents

foreign import mkFileFn """
function mkFileFn(path, contents) {
  var gutil = require('gulp-util');
  return new gutil.File({path: path, contents: contents});
}
""" :: Fn2 String Buffer File
