# Module Documentation

## Module GulpPurescript.ChildProcess

#### `ChildProcess`

``` purescript
data ChildProcess :: !
```


#### `spawn`

``` purescript
spawn :: forall eff. String -> [String] -> Aff (cp :: ChildProcess | eff) String
```



## Module GulpPurescript.FS

#### `FS`

``` purescript
data FS :: !
```


#### `Stream`

``` purescript
data Stream i o
```


#### `createWriteStream`

``` purescript
createWriteStream :: forall eff. String -> Eff (fs :: FS | eff) (Stream String Unit)
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
mkFile :: String -> String -> File
```


#### `filePath`

``` purescript
filePath :: File -> String
```


#### `fileIsNull`

``` purescript
fileIsNull :: File -> Boolean
```


#### `fileIsStream`

``` purescript
fileIsStream :: File -> Boolean
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



## Module GulpPurescript.Multipipe

#### `multipipe2`

``` purescript
multipipe2 :: forall a b c. Stream a b -> Stream b c -> Stream a c
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

#### `isForeignPsc`

``` purescript
instance isForeignPsc :: IsForeign Psc
```


#### `isForeignPscMake`

``` purescript
instance isForeignPscMake :: IsForeign PscMake
```


#### `isForeignPscDocs`

``` purescript
instance isForeignPscDocs :: IsForeign PscDocs
```


#### `isForeignFormat`

``` purescript
instance isForeignFormat :: IsForeign Format
```


#### `pscOptions`

``` purescript
pscOptions :: Foreign -> [String]
```


#### `pscMakeOptions`

``` purescript
pscMakeOptions :: Foreign -> [String]
```


#### `pscDocsOptions`

``` purescript
pscDocsOptions :: Foreign -> [String]
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
type Effects eff = (which :: Which, through2 :: Through2, resolveBin :: ResolveBin, package :: Pkg, os :: OS, logalot :: Logalot, fs :: FS, cp :: ChildProcess | eff)
```


#### `psc`

``` purescript
psc :: forall eff. Foreign -> Eff (Effects eff) (Stream File File)
```


#### `pscMake`

``` purescript
pscMake :: forall eff. Foreign -> Eff (Effects eff) (Stream File Unit)
```


#### `pscDocs`

``` purescript
pscDocs :: forall eff. Foreign -> Eff (Effects eff) (Stream File File)
```


#### `dotPsci`

``` purescript
dotPsci :: forall eff. Eff (Effects eff) (Stream File Unit)
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



## Module GulpPurescript.Through2

#### `Through2`

``` purescript
data Through2 :: !
```


#### `RunAff`

``` purescript
type RunAff eff a = (Error -> Eff eff Unit) -> (a -> Eff eff Unit) -> Aff eff a -> Eff eff Unit
```


#### `objStream`

``` purescript
objStream :: forall eff1 eff2 input output. (input -> Aff eff1 output) -> Eff (through2 :: Through2 | eff2) (Stream input output)
```


#### `accStream`

``` purescript
accStream :: forall eff1 eff2 input output. (input -> Aff eff1 output) -> Eff (through2 :: Through2 | eff2) (Stream input [output])
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




