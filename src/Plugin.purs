module GulpPurescript.Plugin
  ( Effects()
  , psc
  , pscMake
  , pscDocs
  , dotPsci
  ) where

import Control.Monad.Aff (Aff())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error())
import Control.Monad.Error.Class (catchError, throwError)

import Data.Foreign (Foreign())
import Data.Foreign.Class (IsForeign, read, readProp)
import Data.Maybe (Maybe(Just), maybe, fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (tuple2)

import GulpPurescript.Buffer (Buffer(), mkBufferFromString)
import GulpPurescript.ChildProcess (ChildProcess(), spawn)
import GulpPurescript.FS (FS(), Stream(), createWriteStream)
import GulpPurescript.GulpUtil (File(), fileIsNull, fileIsStream, filePath, mkFile, mkPluginError)
import GulpPurescript.Logalot (Logalot(), info)
import GulpPurescript.Minimist (minimist)
import GulpPurescript.Multipipe (multipipe2)
import GulpPurescript.OS (OS(), Platform(Win32), platform)
import GulpPurescript.Options (pscOptionsNoOutput, pscMakeOptions, pscDocsOptions)
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
  , logalot :: Logalot
  , os :: OS
  , package :: Pkg
  , resolveBin :: ResolveBin
  , through2 :: Through2
  , which :: Which
  | eff
  )

nodeCommand = "node"

pursPackage = "purescript"

psciFilename = ".psci"

psciLoadCommand = ":m"

pscCommand = "psc"

pscMakeCommand = "psc-make"

pscDocsCommand = "psc-docs"

pscOutputDefault = "psc.js"

isVerbose = maybe false (\(Argv a) -> a.verbose) (minimist argv)

foreign import cwd "var cwd = process.cwd();" :: String

foreign import argv "var argv = process.argv.slice(2);" :: [String]

pluginError :: forall eff. String -> Aff (Effects eff) Error
pluginError msg = liftEff $ flip mkPluginError msg <$> (maybe "" (\(Package a) -> a.name)) <$> package

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
    mapError _ = pluginError ( "Failed to find " ++ cmd ++ ". " ++
                               "Please ensure it is available on your system."
                             ) >>= throwError

execute :: forall eff. String -> [String] -> Aff (Effects eff) String
execute cmd args = do
  Tuple cmd' args' <- resolve cmd args
  result <- spawn cmd' args'
  return result

pathsStream :: forall eff. Eff (through2 :: Through2 | eff) (Stream File [String])
pathsStream = accStream run
  where run i = if fileIsStream i
                   then pluginError "Streaming is not supported" >>= throwError
                   else pure $ filePath i

psc :: forall eff. Foreign -> Eff (Effects eff) (Stream File File)
psc opts = multipipe2 <$> pathsStream <*> objStream run
  where run i = case pscOptionsNoOutput opts of
                     Tuple out opt ->
                     mkFile (fromMaybe pscOutputDefault out) <$> mkBufferFromString
                                                             <$> execute pscCommand (i <> opt)

pscMake :: forall eff. Foreign -> Eff (Effects eff) (Stream File Unit)
pscMake opts = multipipe2 <$> pathsStream <*> objStream run
  where run i = do output <- execute pscMakeCommand (i <> pscMakeOptions opts)
                   if isVerbose
                      then liftEff $ info $ pscMakeCommand ++ "\n" ++ output
                      else pure unit

pscDocs :: forall eff. Foreign -> Eff (Effects eff) (Stream File File)
pscDocs opts = multipipe2 <$> pathsStream <*> objStream run
  where run i = mkFile "." <$> mkBufferFromString
                           <$> execute pscDocsCommand (pscDocsOptions opts <> i)

dotPsci :: forall eff. Eff (Effects eff) (Stream File Unit)
dotPsci = multipipe2 <$> objStream run <*> createWriteStream psciFilename
  where run i = if fileIsStream i
                   then pluginError "Streaming is not supported" >>= throwError
                   else pure $ psciLoadCommand ++ " " ++ relative cwd (filePath i) ++ "\n"
