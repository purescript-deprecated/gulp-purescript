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
        , ffi :: NullOrUndefined [String]
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
            , ffi :: NullOrUndefined [String]
            }

newtype PscDocs
  = PscDocs { format :: NullOrUndefined Format
            , docgen :: NullOrUndefined Foreign
            }

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

instance isForeignFormat :: IsForeign Format where
  read val = read val >>= (\a -> case a of
                                      "markdown" -> Right Markdown
                                      "etags" -> Right ETags
                                      "ctags" -> Right CTags
                                      a -> Left $ TypeMismatch "Format" a)

mkBoolean :: String -> NullOrUndefined Boolean -> [String]
mkBoolean key opt = maybe [] (\a -> if a then ["--" ++ key] else []) (runNullOrUndefined opt)

mkString :: String -> NullOrUndefined String -> [String]
mkString key opt = maybe [] (\a -> ["--" ++ key ++ "=" ++ a]) (runNullOrUndefined opt)

mkBooleanString :: String -> NullOrUndefined (Either Boolean String) -> [String]
mkBooleanString key opt = maybe [] (either (\a -> mkBoolean key (NullOrUndefined $ Just a))
                                           (\a -> mkString key (NullOrUndefined $ Just a)))
                                   (runNullOrUndefined opt)

mkStringArray :: String -> NullOrUndefined [String] -> [String]
mkStringArray key opt = concat $ mkString key <$> (NullOrUndefined <<< Just)
                                              <$> (fromMaybe [] $ runNullOrUndefined opt)

mkPathArray :: String -> NullOrUndefined [String] -> [String]
mkPathArray key opt = concat $ mkString key <$> (NullOrUndefined <<< Just)
                                            <$> (fromMaybe [] (runNullOrUndefined opt) >>= expandGlob)

mkDocgen :: String -> NullOrUndefined Foreign -> [String]
mkDocgen key opt = concat $ mkString key <$> (NullOrUndefined <<< Just)
                                         <$> (maybe [] parse (runNullOrUndefined opt))
  where
  parse :: Foreign -> [String]
  parse obj = either (const []) id $ parseName obj
                                 <|> parseList obj
                                 <|> parseObj obj
                                 <|> pure []

  parseName :: Foreign -> F [String]
  parseName obj = singleton <$> read obj

  parseList :: Foreign -> F [String]
  parseList obj = read obj

  parseObj :: Foreign -> F [String]
  parseObj obj = do
    modules <- keys obj
    for modules \m -> (\f -> m ++ ":" ++ f) <$> readProp m obj


foreign import expandGlob
  """
  var expandGlob = (function () {
    var glob = require("glob");
    return function (pattern) {
      return glob.sync(pattern);
    };
  }());
  """ :: String -> [String]

mkFormat :: String -> NullOrUndefined Format -> [String]
mkFormat key opt = mkString key (maybe j (\a -> case a of
                                                     Markdown -> i "markdown"
                                                     ETags -> i "etags"
                                                     CTags -> i "ctags") $ runNullOrUndefined opt)
  where i a = NullOrUndefined $ Just a
        j = NullOrUndefined Nothing

foldPscOptions :: Psc -> [String]
foldPscOptions (Psc a) = mkBoolean noPreludeOpt a.noPrelude <>
                         mkBoolean noTcoOpt a.noTco <>
                         mkBoolean noMagicDoOpt a.noMagicDo <>
                         mkBooleanString mainOpt a.main <>
                         mkBoolean noOptsOpt a.noOpts <>
                         mkBoolean verboseErrorsOpt a.verboseErrors <>
                         mkBoolean commentsOpt a.comments <>
                         mkString browserNamespaceOpt a.browserNamespace <>
                         mkStringArray moduleOpt a."module" <>
                         mkStringArray codegenOpt a.codegen <>
                         mkString outputOpt a.output <>
                         mkString externsOpt a.externs <>
                         mkBoolean noPrefixOpt a.noPrefix <>
                         mkPathArray ffiOpt a.ffi

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
        fold (PscMake a) = mkString outputOpt a.output <>
                           mkBoolean noPreludeOpt a.noPrelude <>
                           mkBoolean noTcoOpt a.noTco <>
                           mkBoolean noMagicDoOpt a.noMagicDo <>
                           mkBoolean noOptsOpt a.noOpts <>
                           mkBoolean verboseErrorsOpt a.verboseErrors <>
                           mkBoolean commentsOpt a.comments <>
                           mkBoolean noPrefixOpt a.noPrefix <>
                           mkPathArray ffiOpt a.ffi

pscDocsOptions :: Foreign -> Either ForeignError [String]
pscDocsOptions opts = fold <$> parsed
  where parsed = read opts :: F PscDocs
        fold (PscDocs a) = mkFormat formatOpt a.format <>
                           mkDocgen docgenOpt a.docgen
