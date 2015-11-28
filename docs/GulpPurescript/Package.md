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

##### Instances
``` purescript
IsForeign Package
```

#### `package`

``` purescript
package :: forall eff. Eff (package :: Pkg | eff) (Maybe Package)
```


