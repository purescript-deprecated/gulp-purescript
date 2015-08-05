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


