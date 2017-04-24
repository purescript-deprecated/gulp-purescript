module GulpPurescript.Options
  ( Psci(..)
  , pscOptions
  , pscBundleOptions
  , pscDocsOptions
  , readPsci
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)

import Data.Array (concat, singleton)
import Data.Either (Either(..), either)
import Data.Foreign (F, Foreign, ForeignError(..), fail, readArray, readBoolean, readNullOrUndefined, readString)
import Data.Foreign.Index (readProp)
import Data.Foreign.Keys (keys)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (for, traverse)

srcKey :: String
srcKey = "src"

noOptsOpt :: String
noOptsOpt = "no-opts"

noOptsKey :: String
noOptsKey = camelcaseFn noOptsOpt

noMagicDoOpt :: String
noMagicDoOpt = "no-magic-do"

noMagicDoKey :: String
noMagicDoKey = camelcaseFn noMagicDoOpt

verboseErrorsOpt :: String
verboseErrorsOpt = "verbose-errors"

verboseErrorsKey :: String
verboseErrorsKey = camelcaseFn verboseErrorsOpt

outputOpt :: String
outputOpt = "output"

outputKey :: String
outputKey = outputOpt

namespaceOpt :: String
namespaceOpt = "namespace"

namespaceKey :: String
namespaceKey = namespaceOpt

commentsOpt :: String
commentsOpt = "comments"

commentsKey :: String
commentsKey = commentsOpt

noPrefixOpt :: String
noPrefixOpt = "no-prefix"

noPrefixKey :: String
noPrefixKey = camelcaseFn noPrefixOpt

sourceMapsOpt :: String
sourceMapsOpt = "source-maps"

sourceMapsKey :: String
sourceMapsKey = camelcaseFn sourceMapsOpt

dumpCoreFnOpt :: String
dumpCoreFnOpt = "dump-corefn"

dumpCoreFnKey :: String
dumpCoreFnKey = "dumpCoreFn"

jsonErrorsOpt :: String
jsonErrorsOpt = "json-errors"

jsonErrorsKey :: String
jsonErrorsKey = camelcaseFn jsonErrorsOpt

mainOpt :: String
mainOpt = "main"

mainKey :: String
mainKey = mainOpt

moduleOpt :: String
moduleOpt = "module"

moduleKey :: String
moduleKey = moduleOpt

formatOpt :: String
formatOpt = "format"

formatKey :: String
formatKey = formatOpt

docgenOpt :: String
docgenOpt = "docgen"

docgenKey :: String
docgenKey = docgenOpt

newtype Psc
  = Psc { src :: Either String (Array String)
        , output :: Maybe String
        , verboseErrors :: Maybe Boolean
        , comments :: Maybe Boolean
        , sourceMaps :: Maybe Boolean
        , dumpCoreFn :: Maybe Boolean
        , noPrefix :: Maybe Boolean
        , jsonErrors :: Maybe Boolean
        }

newtype PscBundle
  = PscBundle { src :: Either String (Array String)
              , output :: Maybe String
              , "module" :: Maybe (Either String (Array String))
              , main :: Maybe (Either Boolean String)
              , namespace :: Maybe String
              , sourceMaps :: Maybe Boolean
              }

newtype PscDocs
  = PscDocs { src :: Either String (Array String)
            , format :: Maybe Format
            , docgen :: Maybe Docgen
            }

newtype Psci
  = Psci { src :: Either String (Array String) }

newtype Docgen = Docgen Foreign

newtype PathArray = PathArray (Array String)

data Format = Markdown | ETags | CTags

readPsc :: Foreign -> F Psc
readPsc obj = do
  src <- readSources =<< readProp srcKey obj
  output <- readPropNU readString outputKey obj
  verboseErrors <- readPropNU readBoolean verboseErrorsKey obj
  comments <- readPropNU readBoolean commentsKey obj
  sourceMaps <- readPropNU readBoolean sourceMapsKey obj
  dumpCoreFn <- readPropNU readBoolean dumpCoreFnKey obj
  noPrefix <- readPropNU readBoolean noPrefixKey obj
  jsonErrors <- readPropNU readBoolean jsonErrorsKey obj
  pure $ Psc { src, output, verboseErrors, comments, sourceMaps, dumpCoreFn, noPrefix, jsonErrors }

readPscBundle :: Foreign -> F PscBundle
readPscBundle obj = do
  src <- readSources =<< readProp srcKey obj
  output <- readPropNU readString outputKey obj
  mod <- readPropNU readSources moduleKey obj
  main <- readPropNU (readEither readBoolean readString) mainKey obj
  namespace <- readPropNU readString namespaceKey obj
  sourceMaps <- readPropNU readBoolean sourceMapsKey obj
  pure $ PscBundle { src, output, "module": mod, main, namespace, sourceMaps }

readPscDocs :: Foreign -> F PscDocs
readPscDocs obj = do
  src <- readSources =<< readProp srcKey obj
  format <- readPropNU readFormat formatKey obj
  docgen <- readPropNU readDocgen docgenOpt obj
  pure $ PscDocs { src, format, docgen }

readPsci :: Foreign -> F Psci
readPsci obj = Psci <$> { src: _ } <$> (readSources =<< readProp srcKey obj)

readPathArray :: Foreign -> F PathArray
readPathArray = map PathArray <<< traverse readString <=< readArray

readDocgen :: Foreign -> F Docgen
readDocgen = pure <<< Docgen

readFormat :: Foreign -> F Format
readFormat = readString >=> case _ of
  "markdown" -> pure Markdown
  "etags" -> pure ETags
  "ctags" -> pure CTags
  b -> fail $ TypeMismatch "Format" b

class CommandLineOption a where
  opt :: String -> Maybe a -> Array String

instance commandLineOptionBoolean :: CommandLineOption Boolean where
  opt key = maybe [] (\a -> if a then ["--" <> key] else [])

instance commandLineOptionString :: CommandLineOption String where
  opt key = maybe [] (\a -> ["--" <> key <> "=" <> a])

instance commandLineOptionEither :: (CommandLineOption a, CommandLineOption b) => CommandLineOption (Either a b) where
  opt key = maybe [] (either (opt key <<< Just) (opt key <<< Just))

instance commandLineOptionArray :: (CommandLineOption a) => CommandLineOption (Array a) where
  opt key val = concat $ opt key <$> Just <$> (fromMaybe [] val)

instance commandLineOptionPathArray :: CommandLineOption PathArray where
  opt key = opt key <<< map \(PathArray a) -> a >>= expandGlob

instance commandLineOptionDocgen :: CommandLineOption Docgen where
  opt key = opt key <<< map parseDocgen

instance commandLineOptionFormat :: CommandLineOption Format where
  opt key = opt key <<< map case _ of
    Markdown -> "markdown"
    ETags -> "etags"
    CTags -> "ctags"

parseDocgen :: Docgen -> Array String
parseDocgen (Docgen obj) =
  either (const []) id $ runExcept
    $ parseName obj
    <|> parseList obj
    <|> parseObj obj
    <|> pure []
  where
  parseName :: Foreign -> F (Array String)
  parseName = map singleton <<< readString

  parseList :: Foreign -> F (Array String)
  parseList = traverse readString <=< readArray

  parseObj :: Foreign -> F (Array String)
  parseObj obj' = do
    modules <- keys obj'
    for modules \m -> (\f -> m <> ":" <> f) <$> (readString =<< readProp m obj')

pscOptions :: Foreign -> F (Array String)
pscOptions opts = fold <$> parsed
  where
  parsed :: F Psc
  parsed = readPsc opts

  fold :: Psc -> Array String
  fold (Psc a) = either pure id a.src <>
                 opt outputOpt a.output <>
                 opt verboseErrorsOpt a.verboseErrors <>
                 opt commentsOpt a.comments <>
                 opt sourceMapsOpt a.sourceMaps <>
                 opt dumpCoreFnOpt a.dumpCoreFn <>
                 opt noPrefixOpt a.noPrefix <>
                 opt jsonErrorsOpt a.jsonErrors

pscBundleOptions :: Foreign -> F (Array String)
pscBundleOptions opts = fold <$> parsed
  where
  parsed :: F PscBundle
  parsed = readPscBundle opts

  fold :: PscBundle -> Array String
  fold (PscBundle a) = either pure id a.src <>
                       opt outputOpt a.output <>
                       opt moduleOpt a."module" <>
                       opt mainOpt a.main <>
                       opt namespaceOpt a.namespace <>
                       opt sourceMapsOpt a.sourceMaps

pscDocsOptions :: Foreign -> F (Array String)
pscDocsOptions opts = fold <$> parsed
  where
  parsed :: F PscDocs
  parsed = readPscDocs opts

  fold :: PscDocs -> Array String
  fold (PscDocs a) = either pure id a.src <>
                     opt formatOpt a.format <>
                     opt docgenOpt a.docgen

readEither :: forall left right. (Foreign -> F left) -> (Foreign -> F right) -> Foreign -> F (Either left right)
readEither readL readR a = (Left <$> readL a) <|> (Right <$> readR a)

readEitherNU :: forall left right. (Foreign -> F left) -> (Foreign -> F right) -> Foreign -> F (Maybe (Either left right))
readEitherNU readL readR = traverse (readEither readL readR) <=< readNullOrUndefined

readPropNU :: forall a. (Foreign -> F a) -> String -> Foreign -> F (Maybe a)
readPropNU f k = traverse f <=< readNullOrUndefined <=< readProp k

readSources :: Foreign -> F (Either String (Array String))
readSources = readEither readString (traverse readString <=< readArray)

foreign import expandGlob :: String -> (Array String)

foreign import camelcaseFn :: String -> String
