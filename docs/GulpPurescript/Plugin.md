## Module GulpPurescript.Plugin

#### `Effects`

``` purescript
type Effects eff = (cp :: ChildProcess, glob :: Glob, logalot :: Logalot, os :: OS, package :: Pkg, resolveBin :: ResolveBin, stream :: Stream, which :: Which | eff)
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


