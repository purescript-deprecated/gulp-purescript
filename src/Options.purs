module GulpPurescript.Options
  ( pscOptions
  , pscBundleOptions
  , pscDocsOptions
  ) where

import Control.Alt ((<|>))

import Data.Array (concat, singleton)
import Data.Either (Either(..), either)
import Data.Foreign (Foreign(), ForeignError(TypeMismatch), F())
import Data.Foreign.Class (IsForeign, read, readProp)
import Data.Foreign.Keys (keys)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..), runNullOrUndefined)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (for)
import Data.Tuple (Tuple())
import Data.Tuple.Nested (tuple2)

srcOpt = "src"

srcKey = "src"

noOptsOpt = "no-opts"

noOptsKey = "noOpts"

noMagicDoOpt = "no-magic-do"

noMagicDoKey = "noMagicDo"

noTcoOpt = "no-tco"

noTcoKey = "noTco"

verboseErrorsOpt = "verbose-errors"

verboseErrorsKey = "verboseErrors"

outputOpt = "output"

outputKey = outputOpt

browserNamespaceOpt = "browser-namespace"

browserNamespaceKey = "browserNamespace"

commentsOpt = "comments"

commentsKey = commentsOpt

noPrefixOpt = "no-prefix"

noPrefixKey = "noPrefix"

mainOpt = "main"

mainKey = mainOpt

moduleOpt = "module"

moduleKey = moduleOpt

formatOpt = "format"

formatKey = formatOpt

ffiOpt = "ffi"

ffiKey = ffiOpt

docgenOpt = "docgen"

docgenKey = docgenOpt

newtype Psc
  = Psc { src :: Either String [String]
        , ffi :: NullOrUndefined (Either String [String])
        , output :: NullOrUndefined String
        , noTco :: NullOrUndefined Boolean
        , noMagicDo :: NullOrUndefined Boolean
        , noOpts :: NullOrUndefined Boolean
        , verboseErrors :: NullOrUndefined Boolean
        , comments :: NullOrUndefined Boolean
        , noPrefix :: NullOrUndefined Boolean
        }

newtype PscBundle
  = PscBundle { src :: Either String [String]
              , output :: NullOrUndefined String
              , "module" :: NullOrUndefined (Either String [String])
              , main :: NullOrUndefined (Either Boolean String)
              , browserNamespace :: NullOrUndefined String
              }

newtype PscDocs
  = PscDocs { src :: Either String [String]
            , format :: NullOrUndefined Format
            , docgen :: NullOrUndefined Docgen
            }

newtype DotPsci
  = DotPsci { src :: Either String PathArray
            , ffi :: NullOrUndefined (Either String PathArray)
            }

newtype Docgen = Docgen Foreign

newtype PathArray = PathArray [String]

data Format = Markdown | ETags | CTags

instance isForeignEither :: (IsForeign a, IsForeign b) => IsForeign (Either a b) where
  read a = (Left <$> read a :: F a) <|>
           (Right <$> read a :: F b)

instance isForeignPsc :: IsForeign Psc where
  read obj =
    Psc <$> ({ src: _
             , ffi: _
             , output: _
             , noTco: _
             , noMagicDo: _
             , noOpts: _
             , verboseErrors: _
             , comments: _
             , noPrefix: _
             } <$> readProp srcKey obj
               <*> readProp ffiKey obj
               <*> readProp outputKey obj
               <*> readProp noTcoKey obj
               <*> readProp noMagicDoKey obj
               <*> readProp noOptsKey obj
               <*> readProp verboseErrorsKey obj
               <*> readProp commentsKey obj
               <*> readProp noPrefixKey obj)

instance isForeignPscBundle :: IsForeign PscBundle where
  read obj =
    PscBundle <$> ({ src: _
                   , output: _
                   , "module": _
                   , main: _
                   , browserNamespace: _
                   } <$> readProp srcKey obj
                     <*> readProp outputKey obj
                     <*> readProp moduleKey obj
                     <*> readProp mainKey obj
                     <*> readProp browserNamespaceKey obj)

instance isForeignPscDocs :: IsForeign PscDocs where
  read obj =
    PscDocs <$> ({ src: _
                 , format: _
                 , docgen: _
                 } <$> readProp srcKey obj
                   <*> readProp formatKey obj
                   <*> readProp docgenOpt obj)

instance isForeignDotPsci :: IsForeign DotPsci where
  read obj =
    DotPsci <$> ({ src: _
                 , ffi: _
                 } <$> readProp srcKey obj
                   <*> readProp ffiKey obj)

instance isForeignPathArray :: IsForeign PathArray where
  read val = PathArray <$> read val

instance isForeignDocgen :: IsForeign Docgen where
  read val = Docgen <$> read val

instance isForeignFormat :: IsForeign Format where
  read val = read val >>= (\a -> case a of
                                      "markdown" -> Right Markdown
                                      "etags" -> Right ETags
                                      "ctags" -> Right CTags
                                      a -> Left $ TypeMismatch "Format" a)

class CommandLineOption a where
  opt :: String -> NullOrUndefined a -> [String]

instance commandLineOptionBoolean :: CommandLineOption Boolean where
  opt key val = maybe [] (\a -> if a then ["--" ++ key] else []) (runNullOrUndefined val)

instance commandLineOptionString :: CommandLineOption String where
  opt key val = maybe [] (\a -> ["--" ++ key ++ "=" ++ a]) (runNullOrUndefined val)

instance commandLineOptionEither :: (CommandLineOption a, CommandLineOption b) => CommandLineOption (Either a b) where
  opt key val = maybe [] (either (\a -> opt key (NullOrUndefined $ Just a))
                                 (\a -> opt key (NullOrUndefined $ Just a)))
                      (runNullOrUndefined val)

instance commandLineOptionArray :: (CommandLineOption a) => CommandLineOption [a] where
  opt key val = concat $ opt key <$> (NullOrUndefined <<< Just)
                                 <$> (fromMaybe [] $ runNullOrUndefined val)

instance commandLineOptionPathArray :: CommandLineOption PathArray where
  opt key val = opt key (NullOrUndefined ((\(PathArray a) -> a >>= expandGlob) <$> (runNullOrUndefined val)))

instance commandLineOptionDocgen :: CommandLineOption Docgen where
  opt key val = opt key (NullOrUndefined (parseDocgen <$> (runNullOrUndefined val)))

parseDocgen :: Docgen -> [String]
parseDocgen (Docgen obj) = either (const []) id $ parseName obj
                                              <|> parseList obj
                                              <|> parseObj obj
                                              <|> pure []
  where
    parseName :: Foreign -> F [String]
    parseName obj = singleton <$> read obj

    parseList :: Foreign -> F [String]
    parseList obj = read obj

    parseObj :: Foreign -> F [String]
    parseObj obj = do
      modules <- keys obj
      for modules \m -> (\f -> m ++ ":" ++ f) <$> readProp m obj

instance commandLineOptionFormat :: CommandLineOption Format where
  opt key val = opt key (maybe (NullOrUndefined Nothing)
                               (\a -> case a of
                                           Markdown -> NullOrUndefined (Just "markdown")
                                           ETags -> NullOrUndefined (Just "etags")
                                           CTags -> NullOrUndefined (Just "ctags"))
                               (runNullOrUndefined val))

pscOptions :: Foreign -> Either ForeignError [String]
pscOptions opts = fold <$> parsed
  where parsed = read opts :: F Psc
        fold (Psc a) = either pure id a.src <>
                       opt ffiOpt a.ffi <>
                       opt outputOpt a.output <>
                       opt noTcoOpt a.noTco <>
                       opt noMagicDoOpt a.noMagicDo <>
                       opt noOptsOpt a.noOpts <>
                       opt verboseErrorsOpt a.verboseErrors <>
                       opt commentsOpt a.comments <>
                       opt noPrefixOpt a.noPrefix

pscBundleOptions :: Foreign -> Either ForeignError [String]
pscBundleOptions opts = fold <$> parsed
  where parsed = read opts :: F PscBundle
        fold (PscBundle a) = either pure id a.src <>
                             opt outputOpt a.output <>
                             opt moduleOpt a."module" <>
                             opt mainOpt a.main <>
                             opt browserNamespaceOpt a.browserNamespace

pscDocsOptions :: Foreign -> Either ForeignError [String]
pscDocsOptions opts = fold <$> parsed
  where parsed = read opts :: F PscDocs
        fold (PscDocs a) = either pure id a.src <>
                           opt formatOpt a.format <>
                           opt docgenOpt a.docgen

foreign import expandGlob
  """
  var expandGlob = (function () {
    var glob = require("glob");
    return function (pattern) {
      return glob.sync(pattern);
    };
  }());
  """ :: String -> [String]
