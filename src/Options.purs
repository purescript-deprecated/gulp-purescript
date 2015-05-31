module GulpPurescript.Options
  ( pscOptions
  , pscOptionsNoOutput
  , pscMakeOptions
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

noPreludeOpt = "no-prelude"

noPreludeKey = "noPrelude"

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

codegenOpt = "codegen"

codegenKey = codegenOpt

externsOpt = "externs"

externsKey = externsOpt

formatOpt = "format"

formatKey = formatOpt

ffiOpt = "ffi"

ffiKey = ffiOpt

docgenOpt = "docgen"

docgenKey = docgenOpt

newtype Psc
  = Psc { noPrelude :: NullOrUndefined Boolean
        , noTco :: NullOrUndefined Boolean
        , noMagicDo :: NullOrUndefined Boolean
        , main :: NullOrUndefined (Either Boolean String)
        , noOpts :: NullOrUndefined Boolean
        , verboseErrors :: NullOrUndefined Boolean
        , comments :: NullOrUndefined Boolean
        , browserNamespace :: NullOrUndefined String
        , "module" :: NullOrUndefined [String]
        , codegen :: NullOrUndefined [String]
        , output :: NullOrUndefined String
        , externs :: NullOrUndefined String
        , noPrefix :: NullOrUndefined Boolean
        , ffi :: NullOrUndefined PathArray
        }

newtype PscMake
  = PscMake { noPrelude :: NullOrUndefined Boolean
            , noOpts :: NullOrUndefined Boolean
            , noMagicDo :: NullOrUndefined Boolean
            , noTco :: NullOrUndefined Boolean
            , verboseErrors :: NullOrUndefined Boolean
            , comments :: NullOrUndefined Boolean
            , noPrefix :: NullOrUndefined Boolean
            , output :: NullOrUndefined String
            , ffi :: NullOrUndefined PathArray
            }

newtype PscDocs
  = PscDocs { format :: NullOrUndefined Format
            , docgen :: NullOrUndefined Docgen
            }

newtype Docgen = Docgen Foreign

newtype PathArray = PathArray [String]

data Format = Markdown | ETags | CTags

instance isForeignEither :: (IsForeign a, IsForeign b) => IsForeign (Either a b) where
  read a = (Left <$> read a :: F a) <|>
           (Right <$> read a :: F b)

instance isForeignPsc :: IsForeign Psc where
  read obj =
    Psc <$> ({ noPrelude: _
             , noTco: _
             , noMagicDo: _
             , main: _
             , noOpts: _
             , verboseErrors: _
             , comments: _
             , browserNamespace: _
             , "module": _
             , codegen: _
             , output: _
             , externs: _
             , noPrefix: _
             , ffi: _
             } <$> readProp noPreludeKey obj
               <*> readProp noTcoKey obj
               <*> readProp noMagicDoKey obj
               <*> readProp mainKey obj
               <*> readProp noOptsKey obj
               <*> readProp verboseErrorsKey obj
               <*> readProp commentsKey obj
               <*> readProp browserNamespaceKey obj
               <*> readProp moduleKey obj
               <*> readProp codegenKey obj
               <*> readProp outputKey obj
               <*> readProp externsKey obj
               <*> readProp noPrefixKey obj
               <*> readProp ffiKey obj)

instance isForeignPscMake :: IsForeign PscMake where
  read obj =
    PscMake <$> ({ output: _
                 , noPrelude: _
                 , noTco: _
                 , noMagicDo: _
                 , noOpts: _
                 , verboseErrors: _
                 , comments: _
                 , noPrefix: _
                 , ffi: _
                 } <$> readProp outputKey obj
                   <*> readProp noPreludeKey obj
                   <*> readProp noTcoKey obj
                   <*> readProp noMagicDoKey obj
                   <*> readProp noOptsKey obj
                   <*> readProp verboseErrorsKey obj
                   <*> readProp commentsKey obj
                   <*> readProp noPrefixKey obj
                   <*> readProp ffiKey obj)

instance isForeignPscDocs :: IsForeign PscDocs where
  read obj =
    PscDocs <$> ({ format: _
                 , docgen: _
                 } <$> readProp formatKey obj
                   <*> readProp docgenOpt obj)

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
  opt key val =  opt key (NullOrUndefined ((\(PathArray a) -> a >>= expandGlob) <$> (runNullOrUndefined val)))

foreign import expandGlob
  """
  var expandGlob = (function () {
    var glob = require("glob");
    return function (pattern) {
      return glob.sync(pattern);
    };
  }());
  """ :: String -> [String]

instance commandLineOptionDocgen :: CommandLineOption Docgen where
  opt key val =  opt key (NullOrUndefined (parseDocgen <$> (runNullOrUndefined val)))

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

foldPscOptions :: Psc -> [String]
foldPscOptions (Psc a) = opt noPreludeOpt a.noPrelude <>
                         opt noTcoOpt a.noTco <>
                         opt noMagicDoOpt a.noMagicDo <>
                         opt mainOpt a.main <>
                         opt noOptsOpt a.noOpts <>
                         opt verboseErrorsOpt a.verboseErrors <>
                         opt commentsOpt a.comments <>
                         opt browserNamespaceOpt a.browserNamespace <>
                         opt moduleOpt a."module" <>
                         opt codegenOpt a.codegen <>
                         opt outputOpt a.output <>
                         opt externsOpt a.externs <>
                         opt noPrefixOpt a.noPrefix <>
                         opt ffiOpt a.ffi

pscOptions :: Foreign -> Either ForeignError [String]
pscOptions opts = foldPscOptions <$> (read opts :: F Psc)

pscOptionsNoOutput :: Foreign -> Either ForeignError (Tuple (Maybe String) [String])
pscOptionsNoOutput opts = fold <$> parsed
  where parsed = read opts :: F Psc
        fold (Psc a) = tuple2 (runNullOrUndefined a.output)
                              (foldPscOptions (Psc $ a { output = NullOrUndefined Nothing }))

pscMakeOptions :: Foreign -> Either ForeignError [String]
pscMakeOptions opts = fold <$> parsed
  where parsed = read opts :: F PscMake
        fold (PscMake a) = opt outputOpt a.output <>
                           opt noPreludeOpt a.noPrelude <>
                           opt noTcoOpt a.noTco <>
                           opt noMagicDoOpt a.noMagicDo <>
                           opt noOptsOpt a.noOpts <>
                           opt verboseErrorsOpt a.verboseErrors <>
                           opt commentsOpt a.comments <>
                           opt noPrefixOpt a.noPrefix <>
                           opt ffiOpt a.ffi

pscDocsOptions :: Foreign -> Either ForeignError [String]
pscDocsOptions opts = fold <$> parsed
  where parsed = read opts :: F PscDocs
        fold (PscDocs a) = opt formatOpt a.format <>
                           opt docgenOpt a.docgen
