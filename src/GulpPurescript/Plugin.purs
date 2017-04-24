module GulpPurescript.Plugin
  ( Effects
  , Errorback
  , Callback
  , psc
  , pscBundle
  , pscDocs
  , psci
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Except (runExcept)

import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (either)
import Data.Foreign (F, Foreign, renderForeignError)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(Just))
import Data.String (joinWith, null)
import Data.Tuple (Tuple(..))

import GulpPurescript.Buffer (mkBufferFromString)
import GulpPurescript.ChildProcess (ChildProcess, spawn)
import GulpPurescript.Glob (Glob, globAll)
import GulpPurescript.GulpUtil (File, mkFile, mkPluginError)
import GulpPurescript.Logalot (Logalot, info)
import GulpPurescript.Options (Psci(..), pscOptions, pscBundleOptions, pscDocsOptions, readPsci)
import GulpPurescript.OS (OS, Platform(Win32), platform)
import GulpPurescript.Path (relative)
import GulpPurescript.ResolveBin (ResolveBin, resolveBin)
import GulpPurescript.Stream (Stream, ReadableStream, mkReadableStreamFromAff)
import GulpPurescript.Which (Which, which)

foreign import argv :: Array String

rtsOpts :: Array String
rtsOpts =
  let startIndex = Array.elemIndex "--purs-rts-flags" argv
  in case startIndex of
    Just i -> ["+RTS"] <> Array.drop (i + 1) argv <> ["-RTS"]
    _ -> []

type Effects eff =
  ( cp :: ChildProcess
  , glob :: Glob
  , logalot :: Logalot
  , os :: OS
  , resolveBin :: ResolveBin
  , stream :: Stream
  , which :: Which
  | eff
  )

type Errorback eff = Error -> Eff (Effects eff) Unit

type Callback eff a = a -> Eff (Effects eff) Unit

nodeCommand :: String
nodeCommand = "node"

pursPackage :: String
pursPackage = "purescript"

psciFilename :: String
psciFilename = ".psci"

psciLoadModuleCommand :: String
psciLoadModuleCommand = ":m"

psciLoadForeignCommand :: String
psciLoadForeignCommand = ":f"

pursCommand :: String
pursCommand = "purs"

compileCommand :: String
compileCommand = "compile"

bundleCommand :: String
bundleCommand = "bundle"

docsCommand :: String
docsCommand = "docs"

foreign import cwd :: String

throwPluginError :: forall eff result. String -> Aff (Effects eff) result
throwPluginError = throwError <<< mkPluginError "gulp-purescript"

resolve :: forall eff. String -> Array String -> Aff (Effects eff) (Tuple String (Array String))
resolve cmd args = catchError primary fallback
  where
    primary :: Aff (Effects eff) (Tuple String (Array String))
    primary = do
      bin <- resolveBin pursPackage { executable: cmd }
      os <- liftEff platform
      pure $ case os of
                  Just Win32 -> Tuple nodeCommand ([bin] <> args)
                  _ -> Tuple bin args

    fallback :: Error -> Aff (Effects eff) (Tuple String (Array String))
    fallback _ = (const $ Tuple cmd args) <$> catchError (which cmd) mapError

    mapError :: Error -> Aff (Effects eff) String
    mapError _ = throwPluginError ("Failed to find " <> cmd <> ". " <> "Please ensure it is available on your system.")

execute :: forall eff. String -> Array String -> Aff (Effects eff) String
execute cmd args = do
  Tuple cmd' args' <- resolve pursCommand ([cmd] <> args)
  result <- spawn cmd' args'
  pure result

psc :: forall eff. Foreign -> Eff (Effects eff) (ReadableStream Unit)
psc opts = mkReadableStreamFromAff $ do
  output <- handleRead
                   (execute compileCommand <<< (_ <> rtsOpts))
                   (pscOptions opts)
  if null output
     then pure unit
     else liftEff $ info $ compileCommand <> "\n" <> output

pscBundle :: forall eff. Foreign -> Eff (Effects eff) (ReadableStream File)
pscBundle opts = mkReadableStreamFromAff (handleRead run (pscBundleOptions opts))
  where
    run :: Array String -> Aff (Effects eff) File
    run args = mkFile "." <$> mkBufferFromString
                          <$> execute bundleCommand args

pscDocs :: forall eff. Foreign -> Eff (Effects eff) (ReadableStream File)
pscDocs opts = mkReadableStreamFromAff (handleRead run (pscDocsOptions opts))
  where
    run :: Array String -> Aff (Effects eff) File
    run args = mkFile "." <$> mkBufferFromString
                          <$> execute docsCommand args

psci :: forall eff. Foreign -> Eff (Effects eff) (ReadableStream File)
psci opts = mkReadableStreamFromAff (handleRead run (readPsci opts))
  where
    run :: Psci -> Aff (Effects eff) File
    run (Psci a) = do
      srcs <- globAll (either pure id a.src)

      let lines = loadModule <$> Array.concat srcs
          buffer = mkBufferFromString (joinWith "\n" lines)

      pure (mkFile psciFilename buffer)

    loadModule :: String -> String
    loadModule a = psciLoadModuleCommand <> " " <> relative cwd a

    loadForeign :: String -> String
    loadForeign a = psciLoadForeignCommand <> " " <> relative cwd a

handleRead :: forall a b eff. (a -> Aff (Effects eff) b) -> F a -> Aff (Effects eff) b
handleRead r = either (throwPluginError <<< renderForeignError) r <<< lmap NEL.head <<< runExcept
