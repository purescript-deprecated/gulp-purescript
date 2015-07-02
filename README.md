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

gulp.task('psc', function(){
  return purescript.psc({
    src: 'src/*.purs'
  });
});
```

## API

Refer to the PureScript [compiler usage](https://github.com/purescript/purescript/wiki/Language-Guide:-Getting-Started#compiler-usage) section of the Github wiki for additional details on the behaviour of each option below.

### `purescript.psc(options)`

Invokes the `psc` command. The following options are supported.

###### `src` (String or String Array)

Files to compile. Glob syntax is supported.

###### `ffi` (String or String Array)

Files for code that is included with a `foreign import` in the PureScript source. Glob syntax is supported.

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

### `purescript.pscBundle(options)`

Invokes the `psc-bundle` command. The following options are supported.

###### `src` (String or String Array)

The `psc`-produced JavaScript source files to bundle. Glob syntax is supported.

###### `output` (String)

Sets `--output=<string>` that specifies the output filename for the bundle.

###### `module` (String or String Array)

The name of the module or modules to use as entry points for dead code elimination.

###### `main` (Boolean or String)

Toggles `--main` or sets `--main=<string>` that generates code to run the `main` function in the specified module or the `Main` module by default.

###### `browserNamespace` (String)

Sets `--browser-namespace=<string>` that specifies the namespace that PureScript modules will be exported to when running in the browser.

###### `requirePath` (String)

Sets `--require-path=<string>` that specifies the path prefix to use for `require()` calls in the generated JavaScript.

### `purescript.pscDocs(options)`

Invokes the `psc-docs` command. The following options are supported.

###### `src` (String or String Array)

Files to be used for generating the documentation. Glob syntax is supported.

###### `format` (markdown | etags | ctags)

Sets `--output=<markdown|etags|ctags>` that specifies the output format.

###### `docgen` (String | String Array | Object)

Sets `--docgen=...` that can be used to filter the modules documentation is generated for.

- If a string value is provided, the documentation for that single module will be generated.
- If a list of strings is provided, the documentation for all listed modules will be generated.
- If an object with module name/filename pairs (for example, `{ Module: 'docs/Module.md' }`) is provided, files will be written for each of the modules. In this mode, the task requires no `dest` as no value is returned.

### `purescript.psci(options)`

Generates a `.psci` file.

###### `src` (String or String Array)

Files added to the `.psci` file with the `:m` command. Glob syntax is supported.

###### `ffi` (String or String Array)

Files added to the `.psci` file with the `:f` command. Glob syntax is supported.

## Command line arguments

The `--verbose` argument will display the output during the `psc-make` command. For example `gulp --verbose`.
