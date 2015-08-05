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

##### Instances
``` purescript
instance showPlatform :: Show Platform
instance isForeignPlatform :: IsForeign Platform
```

#### `platform`

``` purescript
platform :: forall eff. Eff (os :: OS | eff) (Maybe Platform)
```


