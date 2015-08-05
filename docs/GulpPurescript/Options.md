## Module GulpPurescript.Options

#### `Psci`

``` purescript
newtype Psci
  = Psci { src :: Either String (Array String), ffi :: NullOrUndefined (Either String (Array String)) }
```

##### Instances
``` purescript
instance isForeignPsci :: IsForeign Psci
```

#### `pscOptions`

``` purescript
pscOptions :: Foreign -> Either ForeignError (Array String)
```

#### `pscBundleOptions`

``` purescript
pscBundleOptions :: Foreign -> Either ForeignError (Array String)
```

#### `pscDocsOptions`

``` purescript
pscDocsOptions :: Foreign -> Either ForeignError (Array String)
```


