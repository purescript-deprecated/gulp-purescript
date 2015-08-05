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


