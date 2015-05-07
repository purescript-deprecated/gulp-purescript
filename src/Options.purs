module GulpPurescript.Options
  ( pscOptions
  , pscOptionsNoOutput
  , pscMakeOptions
  , pscDocsOptions
  ) where

import Control.Alt ((<|>))

import Data.Array (concat)
import Data.Either (Either(..), either)
import Data.Foreign (Foreign(), ForeignError(TypeMismatch), F())
import Data.Foreign.Class (IsForeign, read, readProp)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..), runNullOrUndefined)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
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
  = PscDocs { format :: NullOrUndefined Format }

data Format = Markdown | ETags | CTags

instance isForeignEither :: (IsForeign a, IsForeign b) => IsForeign (Either a b) where
  read a = (Left <$> read a :: F a) <|>
           (Right <$> read a :: F b)

instance isForeignPsc :: IsForeign Psc where
  read obj =
    (\a b c d e f g h i j k l m o ->
    Psc { noPrelude: a
        , noTco: b
        , noMagicDo: c
        , main: d
        , noOpts: e
        , verboseErrors: f
        , comments: g
        , browserNamespace: h
        , "module": i
        , codegen: j
        , output: k
        , externs: l
        , noPrefix: m
        , ffi: o
        }) <$> readProp noPreludeKey obj
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
           <*> readProp ffiKey obj

instance isForeignPscMake :: IsForeign PscMake where
  read obj =
    (\a b c d e f g h i ->
    PscMake { output: a
            , noPrelude: b
            , noTco: c
            , noMagicDo: d
            , noOpts: e
            , verboseErrors: f
            , comments: g
            , noPrefix: h
            , ffi: i
            }) <$> readProp outputKey obj
               <*> readProp noPreludeKey obj
               <*> readProp noTcoKey obj
               <*> readProp noMagicDoKey obj
               <*> readProp noOptsKey obj
               <*> readProp verboseErrorsKey obj
               <*> readProp commentsKey obj
               <*> readProp noPrefixKey obj
               <*> readProp ffiKey obj

instance isForeignPscDocs :: IsForeign PscDocs where
  read obj = (\a -> PscDocs { format: a }) <$> readProp formatKey obj

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
                         mkStringArray ffiOpt a.ffi

pscOptions :: Foreign -> [String]
pscOptions opts = either (const []) foldPscOptions parsed
  where parsed = read opts :: F Psc

pscOptionsNoOutput :: Foreign -> Tuple (Maybe String) [String]
pscOptionsNoOutput opts = either (const $ tuple2 Nothing []) fold parsed
  where parsed = read opts :: F Psc
        fold (Psc a) = tuple2 (runNullOrUndefined a.output)
                              (foldPscOptions (Psc $ a { output = NullOrUndefined Nothing }))

pscMakeOptions :: Foreign -> [String]
pscMakeOptions opts = either (const []) fold parsed
  where parsed = read opts :: F PscMake
        fold (PscMake a) = mkString outputOpt a.output <>
                           mkBoolean noPreludeOpt a.noPrelude <>
                           mkBoolean noTcoOpt a.noTco <>
                           mkBoolean noMagicDoOpt a.noMagicDo <>
                           mkBoolean noOptsOpt a.noOpts <>
                           mkBoolean verboseErrorsOpt a.verboseErrors <>
                           mkBoolean commentsOpt a.comments <>
                           mkBoolean noPrefixOpt a.noPrefix <>
                           mkStringArray ffiOpt a.ffi

pscDocsOptions :: Foreign -> [String]
pscDocsOptions opts = either (const []) fold parsed
  where parsed = read opts :: F PscDocs
        fold (PscDocs a) = mkFormat formatOpt a.format
