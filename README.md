# [gulp](https://github.com/wearefractal/gulp)-purescript

> Runs the [PureScript](http://www.purescript.org) compiler to produce JavaScript files

## Install

Install with [npm](https://npmjs.org/package/gulp-purescript)

```
npm install gulp-purescript --save-dev
```

## Example

```js
var gulp = require('gulp')
  , purescript = require('gulp-purescript')
;
gulp.task('purescript', function(){
  return (
    gulp.src('src/**/*.purs.hs').
      pipe(purescript.psc({noPrelude: true})).
      pipe(gulp.dest('dist/'))
  );
});
```

## API

Refer to the PureScript [usage](http://docs.purescript.org/en/latest/start.html#compiler-usage) section for additional details on the behaviour of each option below.

### purescript.psc(options)

Invokes the `psc` command.

#### options

 - **noPrelude**: Boolean value that toggles `--no-prelude`
  -  Do not include the Prelude in the generated Javascript.
 - **noOpts**: Boolean value that toggles `--no-opts`
  - Disable all optimizations.
 - **noMagicDo**: Boolean value that toggles `--no-magic-do`
  - Turn off optimizations which inline calls to >>= for the Eff monad.
 - **noTco**: Boolean value that toggles `--no-tco`
  - Turn off tail-call elimination.
 - **runtimeTypeChecks**: Boolean value that toggles `--runtime-type-checks`
  - Generate simple runtime type checks for function arguments with simple types.
 - **verboseErrors**: Boolean value that toggles `--verbose-errors`
  - Generate verbose error messages.
 - **main**: Boolean or string value that sets `--main` or `--main=<string>`
  - Generate a call to main in the specified module after all other generated Javascript. Defaults to Main if the option is used but no value is provided.
 - **browserNamespace**: String value that sets `--browser-namespace=<string>`
  - Specify the namespace that PureScript modules will be exported to when running in the browser.
 - **externs**: String value that sets `--externs=<string>`
  - Write a list of foreign imports declarations to the specified file in addition to generating Javascript output.
 - **modules**: String or array value that sets one or more `--module=<string>`
  - If specified, any code which is not referenced transitively from this module will be removed. This argument can be used multiple times.
 - **codegen**: String or array value that sets one or more `--codegen=<string>`
  - A list of modules for which Javascript and externs should be generated. This argument can be used multiple times.
 - **output**: String value that specifies the output file(this won't set'`--output=<string>`)
  - Write the generated Javascript to the specified file.

### purescript.pscMake(options)

Invokes the `psc-make` command.

#### options

 - **noPrelude**: Boolean value that toggles `--no-prelude`
  - Do not include the Prelude in the generated Javascript.
 - **noOpts**: Boolean value that toggles `--no-opts`
  - Disable all optimizations.
 - **noMagicDo**: Boolean value that toggles `--no-magic-do`
  - Turn off optimizations which inline calls to >>= for the Eff monad.
 - **noTco**: Boolean value that toggles `--no-tco`
  - Turn off tail-call elimination.
 - **runtimeTypeChecks**: Boolean value that toggles `--runtime-type-checks`
  - Generate simple runtime type checks for function arguments with simple types.
 - **verboseErrors**: Boolean value that toggles `--verbose-errors`
  - Generate verbose error messages.
 - **browserNamespace**: String value that sets `--browser-namespace=<string>`
  - Specify the namespace that PureScript modules will be exported to when running in the browser.
 - **output**: String value that sets `--output=<string>`
  - Write the generated Javascript to the specified file.

### purescript.dotPsci()

Generates a `.psci` file in the current directory. Each source file is
added with the `:m` command.

### purescript.docgen(options)

Invokes the `docgen` command.

#### options

 - hierarchy: Boolean value that toggles `--hierarchy-images`
