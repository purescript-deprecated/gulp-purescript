module GulpPurescript.Plugin
  ( Effects()
  , Errorback()
  , Callback()
  , psc
  , pscBundle
  , pscDocs
  , psci
  ) where

import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error())
import Control.Monad.Error.Class (catchError, throwError)

import Data.Array (concat)
import Data.Either (Either(..), either)
import Data.Foreign (Foreign())
import Data.Foreign.Class (IsForeign, read, readProp)
import Data.Foreign.NullOrUndefined (runNullOrUndefined)
import Data.Maybe (Maybe(Just), maybe, fromMaybe)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (tuple2)

import GulpPurescript.Buffer (Buffer(), mkBufferFromString)
import GulpPurescript.ChildProcess (ChildProcess(), spawn)
import GulpPurescript.FS (FS(), writeFile)
import GulpPurescript.Glob (Glob(), globAll)
import GulpPurescript.GulpUtil (File(), fileIsNull, fileIsStream, filePath, mkFile, mkPluginError)
import GulpPurescript.Logalot (Logalot(), info)
import GulpPurescript.Minimist (minimist)
import GulpPurescript.Multipipe (multipipe2)
import GulpPurescript.OS (OS(), Platform(Win32), platform)
import GulpPurescript.Options (Psci(..), pscOptions, pscBundleOptions, pscDocsOptions)
import GulpPurescript.Package (Pkg(), Package(..), package)
import GulpPurescript.Path (relative)
import GulpPurescript.ResolveBin (ResolveBin(), resolveBin)
import GulpPurescript.Through2 (Through2(), objStream, accStream)
import GulpPurescript.Which (Which(), which)

newtype Argv = Argv { verbose :: Boolean }

instance isForeignArgv :: IsForeign Argv where
  read obj = (\a -> Argv { verbose: a }) <$> readProp "verbose" obj

type Effects eff =
  ( cp :: ChildProcess
  , fs :: FS
  , glob :: Glob
  , logalot :: Logalot
  , os :: OS
  , package :: Pkg
  , resolveBin :: ResolveBin
  , through2 :: Through2
  , which :: Which
  | eff
  )

type Errorback eff = Error -> Eff (Effects eff) Unit

type Callback eff a = a -> Eff (Effects eff) Unit

nodeCommand = "node"

pursPackage = "purescript"

psciFilename = ".psci"

psciLoadModuleCommand = ":m"

psciLoadForeignCommand = ":f"

pscCommand = "psc"

pscBundleCommand = "psc-bundle"

pscDocsCommand = "psc-docs"

isVerbose = maybe false (\(Argv a) -> a.verbose) (minimist argv)

foreign import cwd "var cwd = process.cwd();" :: String

foreign import argv "var argv = process.argv.slice(2);" :: [String]

throwPluginError :: forall eff. String -> Aff (Effects eff) _
throwPluginError msg = liftEff (flip mkPluginError msg <$> (maybe "" (\(Package a) -> a.name))
                                                       <$> package) >>= throwError

resolve :: forall eff. String -> [String] -> Aff (Effects eff) (Tuple String [String])
resolve cmd args = catchError primary fallback
  where
    primary :: Aff (Effects eff) (Tuple String [String])
    primary = do
      bin <- resolveBin pursPackage { executable: cmd }
      os <- liftEff platform
      return $ case os of
                    Just Win32 -> tuple2 nodeCommand ([bin] <> args)
                    _ -> tuple2 bin args

    fallback :: Error -> Aff (Effects eff) (Tuple String [String])
    fallback _ = (const $ tuple2 cmd args) <$> catchError (which cmd) mapError

    mapError :: Error -> Aff (Effects eff) String
    mapError _ = throwPluginError ("Failed to find " ++ cmd ++ ". " ++ "Please ensure it is available on your system.")

execute :: forall eff. String -> [String] -> Aff (Effects eff) String
execute cmd args = do
  Tuple cmd' args' <- resolve cmd args
  result <- spawn cmd' args'
  return result

psc :: forall eff. Foreign -> Errorback eff -> Callback eff Unit -> Eff (Effects eff) Unit
psc opts eb cb = runAff eb cb $ do
  output <- either (throwPluginError <<< show)
                   (execute pscCommand)
                   (pscOptions opts)
  if isVerbose
    then liftEff $ info $ pscCommand ++ "\n" ++ output
    else pure unit

pscBundle :: forall eff. Foreign -> Errorback eff -> Callback eff Unit -> Eff (Effects eff) Unit
pscBundle opts eb cb = runAff eb cb $ do
  output <- either (throwPluginError <<< show)
                   (execute pscBundleCommand)
                   (pscBundleOptions opts)
  if isVerbose
    then liftEff $ info $ pscCommand ++ "\n" ++ output
    else pure unit

pscDocs :: forall eff. Foreign -> Errorback eff -> Callback eff File -> Eff (Effects eff) Unit
pscDocs opts eb cb = runAff eb cb $ do
  case pscDocsOptions opts of
       Left e  -> throwPluginError (show e)
       Right a -> mkFile "." <$> mkBufferFromString
                             <$> execute pscDocsCommand a

psci :: forall eff. Foreign -> Errorback eff -> Callback eff Unit -> Eff (Effects eff) Unit
psci opts eb cb = runAff eb cb (either (\e -> throwPluginError (show e)) write (read opts))
  where
    write :: Psci -> Aff (Effects eff) Unit
    write (Psci a) = do
      srcs <- globAll (either pure id a.src)
      ffis <- globAll (either pure id (fromMaybe (Right []) (runNullOrUndefined a.ffi)))

      let srcLines = joinWith "\n" (loadModule <$> concat srcs)
          ffiLines = joinWith "\n" (loadForeign <$> concat ffis)

      writeFile psciFilename (srcLines ++ ffiLines)

    loadModule :: String -> String
    loadModule a = psciLoadModuleCommand ++ " " ++ relative cwd a

    loadForeign :: String -> String
    loadForeign a = psciLoadForeignCommand ++ " " ++ relative cwd a
