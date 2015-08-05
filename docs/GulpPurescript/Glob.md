## Module GulpPurescript.Glob

#### `Glob`

``` purescript
data Glob :: !
```

#### `glob`

``` purescript
glob :: forall eff. String -> Aff (glob :: Glob | eff) (Array String)
```

#### `globAll`

``` purescript
globAll :: forall eff. Array String -> Aff (glob :: Glob | eff) (Array (Array String))
```


