# [gulp](https://github.com/wearefractal/gulp)-purescript

> Runs the [PureScript](http://www.purescript.org) compiler to produce JavaScript files

## Install

Install with [npm](https://npmjs.org/package/gulp-purescript)

```
npm install gulp-purescript --save-dev
```

## Binaries

This plugin requires that the PureScript binaries first be installed. The binaries may be installed using the [purescript](https://www.npmjs.com/package/purescript) NPM package or as described on the PureScript [installation](https://github.com/purescript/purescript/wiki/Language-Guide:-Getting-Started#installation) section of the GitHub wiki.

## Example

```js
var gulp = require('gulp');

var purescript = require('gulp-purescript');

gulp.task('purescript', function(){
  return gulp.src('src/**/*.purs').
         pipe(purescript.psc({noPrelude: true})).
         pipe(gulp.dest('build'));
});
```

## API

Refer to the PureScript [compiler usage](https://github.com/purescript/purescript/wiki/Language-Guide:-Getting-Started#compiler-usage) section of the Github wiki for additional details on the behaviour of each option below.

### purescript.psc(options)

Invokes the `psc` command.

#### options

 - **noPrelude**: Boolean value that toggles `--no-prelude`
  -  Do not include the Prelude in the generated Javascript.
 - **noTco**: Boolean value that toggles `--no-tco`
  - Turn off tail-call elimination.
 - **noMagicDo**: Boolean value that toggles `--no-magic-do`
  - Turn off optimizations which inline calls to >>= for the Eff monad.
 - **main**: Boolean or string value that sets `--main` or `--main=<string>`
  - Generate a call to main in the specified module after all other generated Javascript. Defaults to Main if the option is used but no value is provided.
 - **noOpts**: Boolean value that toggles `--no-opts`
  - Disable all optimizations.
 - **verboseErrors**: Boolean value that toggles `--verbose-errors`
  - Generate verbose error messages.
 - **comments**: Boolean value that toggles `--comments`
  - Include comments in generated code.
 - **browserNamespace**: String value that sets `--browser-namespace=<string>`
  - Specify the namespace that PureScript modules will be exported to when running in the browser.
 - **externs**: String value that sets `--externs=<string>`
  - Write a list of foreign imports declarations to the specified file in addition to generating Javascript output.
 - **module**: Array of string values that sets one or more `--module=<string>`
  - If specified, any code which is not referenced transitively from this module will be removed.
 - **codegen**: Array of string values that sets one or more `--codegen=<string>`
  - A array of modules for which JavaScript and externs should be generated.
 - **output**: String value that specifies the output file. Note that this will not set `--output=<string>` because the resulting file is piped through the Gulp stream.
  - Write the generated Javascript to the specified file.
 - **noPrefix**: Boolean value that toggles `--no-prefix`
  - Do not include the comment header.

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
 - **verboseErrors**: Boolean value that toggles `--verbose-errors`
  - Generate verbose error messages.
 - **browserNamespace**: String value that sets `--browser-namespace=<string>`
  - Specify the namespace that PureScript modules will be exported to when running in the browser.
 - **output**: String value that sets `--output=<string>`
  - Write the generated Javascript to the specified file.

### purescript.pscDocs(options)

Invokes the `pscDocs` command.

#### options

 - hierarchy: Boolean value that toggles `--hierarchy-images`

### purescript.dotPsci()

Generates a `.psci` file in the current directory. Each source file is
added with the `:m` command.

## Command line arguments

The `--verbose` argument will display the output during the `psc-make`
command. For example `gulp --verbose`.
