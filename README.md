# [gulp](https://github.com/wearefractal/gulp)-purescript

> Runs the [PureScript](http://www.purescript.org) compiler to produce JavaScript files

## Install

Install with [npm](https://npmjs.org/package/gulp-purescript)

```
npm install gulp-purescript --save-dev
```

## Binaries

This plugin requires that the PureScript binaries first be installed. The binaries may be installed using the [purescript](https://www.npmjs.com/package/purescript) NPM package or as described on the PureScript [installation](https://github.com/purescript/purescript/wiki/Language-Guide:-Getting-Started#installation) section of the GitHub wiki.

## Basic example

```js
var gulp = require('gulp');

var purescript = require('gulp-purescript');

gulp.task('psc', function(){
  return purescript.psc({
    src: 'src/*.purs'
  });
});
```

There is also [a more complete example](#full-example) that makes use of all the provided tasks in a common setup.

## API

Refer to the PureScript [compiler usage](https://github.com/purescript/purescript/wiki/Language-Guide:-Getting-Started#compiler-usage) section of the Github wiki for additional details on the behaviour of each option below.

Options can be passed to the Haskell runtime system for `psc` by passing a `--psc-rts-opts` argument to `gulp`. Any values that follow this flag will be passed through to the runtime. There is no need to include `+RTS`/`-RTS` options as these are inserted automatically. See [the GHC documentation](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runtime-control.html#rts-opts-cmdline) for information on the available RTS options.

### `purescript.psc(options)`

Invokes the `psc` command. The following options are supported.

###### `src` (String or String Array)

Files to compile. Glob syntax is supported.

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

###### `sourceMaps` (Boolean)

Toggles `--source-maps` that generates source maps.

###### `jsonErrors` (Boolean)

Toggles `--json-errors` that prints errors to stderr as JSON.

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

###### `namespace` (String)

Sets `--namespace=<string>` that specifies the namespace that PureScript modules will be exported to when running in the browser.

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

## Full example

This example will make and bundle the code, run tests, and produce a `.psci` file and documentation for a project using the common `bower_components`/`src` file layout.

``` js
var gulp = require("gulp");
var purescript = require("gulp-purescript");
var run = require("gulp-run");

var sources = [
  "src/**/*.purs",
  "bower_components/purescript-*/src/**/*.purs",
];

gulp.task("make", function () {
  return purescript.psc({ src: sources });
});

gulp.task("bundle", ["make"], function () {
  return purescript.pscBundle({ src: "output/**/*.js", output: "dist/bundle.js" });
});

gulp.task("docs", function () {
  return purescript.pscDocs({
      src: sources,
      docgen: {
        "Name.Of.Module1": "docs/Name/Of/Module1.md",
        "Name.Of.Module2": "docs/Name/Of/Module2.md"
      }
    });
});

gulp.task("dotpsci", function () {
  return purescript.psci({ src: sources })
    .pipe(gulp.dest("."));
});

gulp.task("test", ["make"], function() {
  return purescript.pscBundle({ src: "output/**/*.js", main: "Test.Main" })
    .pipe(run("node"));
});

gulp.task("default", ["bundle", "docs", "dotpsci", "test"]);
```
