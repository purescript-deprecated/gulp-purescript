# Module Documentation

## Module GulpPurescript.Buffer

#### `Buffer`

``` purescript
data Buffer
```


#### `mkBufferFromString`

``` purescript
mkBufferFromString :: String -> Buffer
```



## Module GulpPurescript.ChildProcess

#### `ChildProcess`

``` purescript
data ChildProcess :: !
```


#### `spawn`

``` purescript
spawn :: forall eff. String -> [String] -> Aff (cp :: ChildProcess | eff) String
```



## Module GulpPurescript.Glob

#### `Glob`

``` purescript
data Glob :: !
```


#### `glob`

``` purescript
glob :: forall eff. String -> Aff (glob :: Glob | eff) [String]
```


#### `globAll`

``` purescript
globAll :: forall eff. [String] -> Aff (glob :: Glob | eff) [[String]]
```



## Module GulpPurescript.GulpUtil

#### `File`

``` purescript
data File
```


#### `mkPluginError`

``` purescript
mkPluginError :: String -> String -> Error
```


#### `mkFile`

``` purescript
mkFile :: String -> Buffer -> File
```



## Module GulpPurescript.Logalot

#### `Logalot`

``` purescript
data Logalot :: !
```


#### `info`

``` purescript
info :: forall eff. String -> Eff (logalot :: Logalot | eff) Unit
```



## Module GulpPurescript.Minimist

#### `minimist`

``` purescript
minimist :: forall a. (IsForeign a) => [String] -> Maybe a
```



## Module GulpPurescript.OS

#### `OS`

``` purescript
data OS :: !
```


#### `Platform`

``` purescript
data Platform
  = Darwin 
  | Linux 
  | Win32 
```


#### `showPlatform`

``` purescript
instance showPlatform :: Show Platform
```


#### `isForeignPlatform`

``` purescript
instance isForeignPlatform :: IsForeign Platform
```


#### `platform`

``` purescript
platform :: forall eff. Eff (os :: OS | eff) (Maybe Platform)
```



## Module GulpPurescript.Options

#### `Psci`

``` purescript
newtype Psci
  = Psci { ffi :: NullOrUndefined (Either String [String]), src :: Either String [String] }
```


#### `isForeignEither`

``` purescript
instance isForeignEither :: (IsForeign a, IsForeign b) => IsForeign (Either a b)
```


#### `isForeignPsc`

``` purescript
instance isForeignPsc :: IsForeign Psc
```


#### `isForeignPscBundle`

``` purescript
instance isForeignPscBundle :: IsForeign PscBundle
```


#### `isForeignPscDocs`

``` purescript
instance isForeignPscDocs :: IsForeign PscDocs
```


#### `isForeignPsci`

``` purescript
instance isForeignPsci :: IsForeign Psci
```


#### `isForeignPathArray`

``` purescript
instance isForeignPathArray :: IsForeign PathArray
```


#### `isForeignDocgen`

``` purescript
instance isForeignDocgen :: IsForeign Docgen
```


#### `isForeignFormat`

``` purescript
instance isForeignFormat :: IsForeign Format
```


#### `commandLineOptionBoolean`

``` purescript
instance commandLineOptionBoolean :: CommandLineOption Boolean
```


#### `commandLineOptionString`

``` purescript
instance commandLineOptionString :: CommandLineOption String
```


#### `commandLineOptionEither`

``` purescript
instance commandLineOptionEither :: (CommandLineOption a, CommandLineOption b) => CommandLineOption (Either a b)
```


#### `commandLineOptionArray`

``` purescript
instance commandLineOptionArray :: (CommandLineOption a) => CommandLineOption [a]
```


#### `commandLineOptionPathArray`

``` purescript
instance commandLineOptionPathArray :: CommandLineOption PathArray
```


#### `commandLineOptionDocgen`

``` purescript
instance commandLineOptionDocgen :: CommandLineOption Docgen
```


#### `commandLineOptionFormat`

``` purescript
instance commandLineOptionFormat :: CommandLineOption Format
```


#### `pscOptions`

``` purescript
pscOptions :: Foreign -> Either ForeignError [String]
```


#### `pscBundleOptions`

``` purescript
pscBundleOptions :: Foreign -> Either ForeignError [String]
```


#### `pscDocsOptions`

``` purescript
pscDocsOptions :: Foreign -> Either ForeignError [String]
```



## Module GulpPurescript.Package

#### `Pkg`

``` purescript
data Pkg :: !
```


#### `Package`

``` purescript
newtype Package
  = Package { name :: String }
```


#### `isForeignPackage`

``` purescript
instance isForeignPackage :: IsForeign Package
```


#### `package`

``` purescript
package :: forall eff. Eff (package :: Pkg | eff) (Maybe Package)
```



## Module GulpPurescript.Path

#### `relative`

``` purescript
relative :: String -> String -> String
```



## Module GulpPurescript.Plugin

#### `isForeignArgv`

``` purescript
instance isForeignArgv :: IsForeign Argv
```


#### `Effects`

``` purescript
type Effects eff = (which :: Which, stream :: Stream, resolveBin :: ResolveBin, package :: Pkg, os :: OS, logalot :: Logalot, glob :: Glob, cp :: ChildProcess | eff)
```


#### `Errorback`

``` purescript
type Errorback eff = Error -> Eff (Effects eff) Unit
```


#### `Callback`

``` purescript
type Callback eff a = a -> Eff (Effects eff) Unit
```


#### `psc`

``` purescript
psc :: forall eff. Foreign -> Eff (Effects eff) (ReadableStream Unit)
```


#### `pscBundle`

``` purescript
pscBundle :: forall eff. Foreign -> Eff (Effects eff) (ReadableStream File)
```


#### `pscDocs`

``` purescript
pscDocs :: forall eff. Foreign -> Eff (Effects eff) (ReadableStream File)
```


#### `psci`

``` purescript
psci :: forall eff. Foreign -> Eff (Effects eff) (ReadableStream File)
```



## Module GulpPurescript.ResolveBin

#### `ResolveBin`

``` purescript
data ResolveBin :: !
```


#### `Options`

``` purescript
type Options = { executable :: String }
```


#### `resolveBin`

``` purescript
resolveBin :: forall eff. String -> Options -> Aff (resolveBin :: ResolveBin | eff) String
```



## Module GulpPurescript.Stream

#### `Stream`

``` purescript
data Stream :: !
```


#### `ReadableStream`

``` purescript
data ReadableStream out
```


#### `mkReadableStreamFromAff`

``` purescript
mkReadableStreamFromAff :: forall eff1 eff2 out. Aff eff1 out -> Eff (stream :: Stream | eff2) (ReadableStream out)
```



## Module GulpPurescript.Which

#### `Which`

``` purescript
data Which :: !
```


#### `which`

``` purescript
which :: forall eff. String -> Aff (which :: Which | eff) String
```




