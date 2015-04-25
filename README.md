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

### `purescript.psc(options)`

Invokes the `psc` command. The following options are supported.

###### `noPrelude` (Boolean)

Toggles `--no-prelude` that omits the Prelude.

###### `noTco` (Boolean)

Toggles `--no-tco` that disables tail-call optimizations.

###### `noMagicDo` (Boolean)

Toggles `--no-magic-do` that disables optimizations overloading the do keyword generating efficient code for the `Eff` monad.

###### `main` (Boolean or String)

Toggles `--main` or sets `--main=<string>` that generates code to run the `main` function in the specified module or the `Main` module by default.

###### `noOpts` (Boolean)

Toggles `--no-opts` that skips the optimization phase.

###### `verboseErrors` (Boolean)

Toggles `--verbose-errors` that displays verbose error messages.

###### `comments` (Boolean)

Toggles `--comments` that includes comments in generated code.

###### `browserNamespace` (String)

Sets `--browser-namespace=<string>` that specifies the namespace that PureScript modules will be exported to when running in the browser.

###### `externs` (String)

Sets `--externs=<string>` that specifies the externs file.

###### `module` (String Array)

Sets one or more `--module=<string>` that enables dead code elimination, removing all code without a transitive dependency of one of the specified modules.

###### `codegen` (String Array)

Sets one or more `--codegen=<string>` that generates code and extenrs for the specified modules.

###### `output` (String)

Sets the path value of the [File](https://github.com/wearefractal/vinyl) passed through the Gulp stream. Note that this will not set `--output=<string>`.

###### `noPrefix` (Boolean)

Toggles `--no-prefix` that does not include the comment header.

### `purescript.pscMake(options)`

Invokes the `psc-make` command. The following options are supported.

###### `noPrelude` (Boolean)

Toggles `--no-prelude` that omits the Prelude.

###### `noTco` (Boolean)

Toggles `--no-tco` that disables tail-call optimizations.

###### `noMagicDo` (Boolean)

Toggles `--no-magic-do` that disables optimizations overloading the do keyword generating efficient code for the `Eff` monad.

###### `noOpts` (Boolean)

Toggles `--no-opts` that skips the optimization phase.

###### `verboseErrors` (Boolean)

Toggles `--verbose-errors` that displays verbose error messages.

###### `comments` (Boolean)

Toggles `--comments` that includes comments in generated code.

###### `output` (String)

Sets `--output=<string>` the specifies the output directory, `output` by default.

###### `noPrefix` (Boolean)

Toggles `--no-prefix` that does not include the comment header.

### `purescript.pscDocs(options)`

Invokes the `pscDocs` command. The following options are supported.

###### `format` (markdown | etags | ctags)

Sets `--output=<markdown|etags|ctags>` that specifies the output format.

### `purescript.dotPsci()`

Generates a `.psci` file in the current directory. Each source file is added with the `:m` command.

## Command line arguments

The `--verbose` argument will display the output during the `psc-make` command. For example `gulp --verbose`.
