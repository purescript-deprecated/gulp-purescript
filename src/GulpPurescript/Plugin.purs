module GulpPurescript.Plugin
  ( Effects
  , Errorback
  , Callback
  , psc
  , pscBundle
  , pscDocs
  , psci
  ) where

import Prelude (Unit, ($), (<>), (<$>), (<*>), (<<<), (>>=), (+), bind, const, id, pure, show, unit)

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Error.Class (catchError, throwError)

import Data.Array (elemIndex, drop, concat) as Array
import Data.Either (either)
import Data.Foreign (Foreign)
import Data.Foreign.Class (read)
import Data.Maybe (Maybe(Just))
import Data.String (joinWith, null)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (tuple2)

import GulpPurescript.Buffer (mkBufferFromString)
import GulpPurescript.ChildProcess (ChildProcess, spawn)
import GulpPurescript.Glob (Glob, globAll)
import GulpPurescript.GulpUtil (File, mkFile, mkPluginError)
import GulpPurescript.Logalot (Logalot, info)
import GulpPurescript.OS (OS, Platform(Win32), platform)
import GulpPurescript.Options (Psci(..), pscOptions, pscBundleOptions, pscDocsOptions)
import GulpPurescript.Path (relative)
import GulpPurescript.ResolveBin (ResolveBin, resolveBin)
import GulpPurescript.Stream (Stream, ReadableStream, mkReadableStreamFromAff)
import GulpPurescript.Which (Which, which)

foreign import argv :: Array String

rtsOpts :: Array String
rtsOpts =
  let startIndex = Array.elemIndex "--psc-rts-flags" argv
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

pscCommand :: String
pscCommand = "psc"

pscBundleCommand :: String
pscBundleCommand = "psc-bundle"

pscDocsCommand :: String
pscDocsCommand = "psc-docs"

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
                  Just Win32 -> tuple2 nodeCommand ([bin] <> args)
                  _ -> tuple2 bin args

    fallback :: Error -> Aff (Effects eff) (Tuple String (Array String))
    fallback _ = (const $ tuple2 cmd args) <$> catchError (which cmd) mapError

    mapError :: Error -> Aff (Effects eff) String
    mapError _ = throwPluginError ("Failed to find " <> cmd <> ". " <> "Please ensure it is available on your system.")

execute :: forall eff. String -> Array String -> Aff (Effects eff) String
execute cmd args = do
  Tuple cmd' args' <- resolve cmd args
  result <- spawn cmd' args'
  pure result

psc :: forall eff. Foreign -> Eff (Effects eff) (ReadableStream Unit)
psc opts = mkReadableStreamFromAff $ do
  output <- either (throwPluginError <<< show)
                   (execute pscCommand <<< (_ <> rtsOpts))
                   (pscOptions opts)
  if null output
     then pure unit
     else liftEff $ info $ pscCommand <> "\n" <> output

pscBundle :: forall eff. Foreign -> Eff (Effects eff) (ReadableStream File)
pscBundle opts = mkReadableStreamFromAff (either (throwPluginError <<< show) run (pscBundleOptions opts))
  where
    run :: Array String -> Aff (Effects eff) File
    run args = mkFile "." <$> mkBufferFromString
                          <$> execute pscBundleCommand args

pscDocs :: forall eff. Foreign -> Eff (Effects eff) (ReadableStream File)
pscDocs opts = mkReadableStreamFromAff (either (throwPluginError <<< show) run (pscDocsOptions opts))
  where
    run :: Array String -> Aff (Effects eff) File
    run args = mkFile "." <$> mkBufferFromString
                          <$> execute pscDocsCommand args

psci :: forall eff. Foreign -> Eff (Effects eff) (ReadableStream File)
psci opts = mkReadableStreamFromAff (either (throwPluginError <<< show) run (read opts))
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
