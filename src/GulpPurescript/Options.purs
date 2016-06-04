module GulpPurescript.Options
  ( Psci(..)
  , pscOptions
  , pscBundleOptions
  , pscDocsOptions
  ) where

import Prelude ((<>), (<$>), (<*>), (<<<), ($), (>>=), bind, const, id, pure)

import Control.Alt ((<|>))

import Data.Array (concat, singleton)
import Data.Either (Either(..), either)
import Data.Foreign (Foreign, ForeignError(TypeMismatch), F)
import Data.Foreign.Class (class IsForeign, read, readProp)
import Data.Foreign.Keys (keys)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..), unNullOrUndefined)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (for)

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

noTcoOpt :: String
noTcoOpt = "no-tco"

noTcoKey :: String
noTcoKey = camelcaseFn noTcoOpt

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
        , output :: NullOrUndefined String
        , noTco :: NullOrUndefined Boolean
        , noMagicDo :: NullOrUndefined Boolean
        , noOpts :: NullOrUndefined Boolean
        , verboseErrors :: NullOrUndefined Boolean
        , comments :: NullOrUndefined Boolean
        , noPrefix :: NullOrUndefined Boolean
        , sourceMaps :: NullOrUndefined Boolean
        , jsonErrors :: NullOrUndefined Boolean
        }

newtype PscBundle
  = PscBundle { src :: Either String (Array String)
              , output :: NullOrUndefined String
              , "module" :: NullOrUndefined (Either String (Array String))
              , main :: NullOrUndefined (Either Boolean String)
              , namespace :: NullOrUndefined String
              }

newtype PscDocs
  = PscDocs { src :: Either String (Array String)
            , format :: NullOrUndefined Format
            , docgen :: NullOrUndefined Docgen
            }

newtype Psci
  = Psci { src :: Either String (Array String) }

newtype Docgen = Docgen Foreign

newtype PathArray = PathArray (Array String)

data Format = Markdown | ETags | CTags

instance isForeignPsc :: IsForeign Psc where
  read obj =
    Psc <$> ({ src: _
             , output: _
             , noTco: _
             , noMagicDo: _
             , noOpts: _
             , verboseErrors: _
             , comments: _
             , noPrefix: _
             , sourceMaps: _
             , jsonErrors: _
             } <$> (readProp srcKey obj >>= readEither)
               <*> readProp outputKey obj
               <*> readProp noTcoKey obj
               <*> readProp noMagicDoKey obj
               <*> readProp noOptsKey obj
               <*> readProp verboseErrorsKey obj
               <*> readProp commentsKey obj
               <*> readProp noPrefixKey obj
               <*> readProp sourceMapsKey obj
               <*> readProp jsonErrorsKey obj)

instance isForeignPscBundle :: IsForeign PscBundle where
  read obj =
    PscBundle <$> ({ src: _
                   , output: _
                   , "module": _
                   , main: _
                   , namespace: _
                   } <$> (readProp srcKey obj >>= readEither)
                     <*> readProp outputKey obj
                     <*> (readProp moduleKey obj >>= readEitherNU)
                     <*> (readProp mainKey obj >>= readEitherNU)
                     <*> readProp namespaceKey obj)

instance isForeignPscDocs :: IsForeign PscDocs where
  read obj =
    PscDocs <$> ({ src: _
                 , format: _
                 , docgen: _
                 } <$> (readProp srcKey obj >>= readEither)
                   <*> readProp formatKey obj
                   <*> readProp docgenOpt obj)

instance isForeignPsci :: IsForeign Psci where
  read obj = Psci <$> ({ src: _ } <$> (readProp srcKey obj >>= readEither))

instance isForeignPathArray :: IsForeign PathArray where
  read val = PathArray <$> read val

instance isForeignDocgen :: IsForeign Docgen where
  read val = Docgen <$> read val

instance isForeignFormat :: IsForeign Format where
  read val = read val >>= (\a -> case a of
                                      "markdown" -> Right Markdown
                                      "etags" -> Right ETags
                                      "ctags" -> Right CTags
                                      b -> Left $ TypeMismatch "Format" b)

class CommandLineOption a where
  opt :: String -> NullOrUndefined a -> Array String

instance commandLineOptionBoolean :: CommandLineOption Boolean where
  opt key val = maybe [] (\a -> if a then ["--" <> key] else []) (unNullOrUndefined val)

instance commandLineOptionString :: CommandLineOption String where
  opt key val = maybe [] (\a -> ["--" <> key <> "=" <> a]) (unNullOrUndefined val)

instance commandLineOptionEither :: (CommandLineOption a, CommandLineOption b) => CommandLineOption (Either a b) where
  opt key val = maybe [] (either (\a -> opt key (NullOrUndefined $ Just a))
                                 (\a -> opt key (NullOrUndefined $ Just a)))
                      (unNullOrUndefined val)

instance commandLineOptionArray :: (CommandLineOption a) => CommandLineOption (Array a) where
  opt key val = concat $ opt key <$> (NullOrUndefined <<< Just)
                                 <$> (fromMaybe [] $ unNullOrUndefined val)

instance commandLineOptionPathArray :: CommandLineOption PathArray where
  opt key val = opt key (NullOrUndefined ((\(PathArray a) -> a >>= expandGlob) <$> (unNullOrUndefined val)))

instance commandLineOptionDocgen :: CommandLineOption Docgen where
  opt key val = opt key (NullOrUndefined (parseDocgen <$> (unNullOrUndefined val)))

parseDocgen :: Docgen -> Array String
parseDocgen (Docgen obj) = either (const []) id $ parseName obj
                                              <|> parseList obj
                                              <|> parseObj obj
                                              <|> pure []
  where
  parseName :: Foreign -> F (Array String)
  parseName obj = singleton <$> read obj

  parseList :: Foreign -> F (Array String)
  parseList obj = read obj

  parseObj :: Foreign -> F (Array String)
  parseObj obj = do
    modules <- keys obj
    for modules \m -> (\f -> m <> ":" <> f) <$> readProp m obj

instance commandLineOptionFormat :: CommandLineOption Format where
  opt key val = opt key (maybe (NullOrUndefined Nothing)
                               (\a -> case a of
                                           Markdown -> NullOrUndefined (Just "markdown")
                                           ETags -> NullOrUndefined (Just "etags")
                                           CTags -> NullOrUndefined (Just "ctags"))
                               (unNullOrUndefined val))

pscOptions :: Foreign -> Either ForeignError (Array String)
pscOptions opts = fold <$> parsed
  where
  parsed :: F Psc
  parsed = read opts

  fold :: Psc -> Array String
  fold (Psc a) = either pure id a.src <>
                 opt outputOpt a.output <>
                 opt noTcoOpt a.noTco <>
                 opt noMagicDoOpt a.noMagicDo <>
                 opt noOptsOpt a.noOpts <>
                 opt verboseErrorsOpt a.verboseErrors <>
                 opt commentsOpt a.comments <>
                 opt noPrefixOpt a.noPrefix <>
                 opt sourceMapsOpt a.sourceMaps <>
                 opt jsonErrorsOpt a.jsonErrors

pscBundleOptions :: Foreign -> Either ForeignError (Array String)
pscBundleOptions opts = fold <$> parsed
  where
  parsed :: F PscBundle
  parsed = read opts

  fold :: PscBundle -> Array String
  fold (PscBundle a) = either pure id a.src <>
                       opt outputOpt a.output <>
                       opt moduleOpt a."module" <>
                       opt mainOpt a.main <>
                       opt namespaceOpt a.namespace

pscDocsOptions :: Foreign -> Either ForeignError (Array String)
pscDocsOptions opts = fold <$> parsed
  where
  parsed :: F PscDocs
  parsed = read opts

  fold :: PscDocs -> Array String
  fold (PscDocs a) = either pure id a.src <>
                     opt formatOpt a.format <>
                     opt docgenOpt a.docgen

readEither :: forall left right. (IsForeign left, IsForeign right) => Foreign -> F (Either left right)
readEither a = (Left <$> read a) <|> (Right <$> read a)

readEitherNU :: forall left right. (IsForeign left, IsForeign right) => NullOrUndefined Foreign -> F (NullOrUndefined (Either left right))
readEitherNU a @ (NullOrUndefined Nothing) = pure (NullOrUndefined Nothing)
readEitherNU (NullOrUndefined (Just a)) = (NullOrUndefined <<< Just) <$> readEither a

foreign import expandGlob :: String -> (Array String)

foreign import camelcaseFn :: String -> String
