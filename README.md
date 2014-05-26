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

Refer to the PureScript [usage](http://docs.purescript.org/en/latest/intro.html#usage) section for additional details on the behaviour of each option below.

### purescript.psc(options)

Invokes the `psc` command.

#### options

 - noPrelude: Boolean value that toggles `--no-prelude`
 - noOpts: Boolean value that toggles `--no-opts`
 - noMagicDo: Boolean value that toggles `--no-magic-do`
 - noTco: Boolean value that toggles `--no-tco`
 - runtimeTypeChecks: Boolean value that toggles `--runtime-type-checks`
 - verboseErrors: Boolean value that toggles `--verbose-errors`
 - main: Boolean or string value that sets `--main` or `--main=<string>`
 - browserNamespace: String value that sets `--browser-namespace=<string>`
 - externs: String value that sets `--externs=<string>`
 - modules: String or array value that sets one or more `--module=<string>`
 - codegen: String or array value that sets one or more `--codegen=<string>`
 - output: String value that specifies the output file(this won't set'`--output=<string>`)

### purescript.pscMake(options)

Invokes the `psc-make` command.

#### options

 - noPrelude: Boolean value that toggles `--no-prelude`
 - noOpts: Boolean value that toggles `--no-opts`
 - noMagicDo: Boolean value that toggles `--no-magic-do`
 - noTco: Boolean value that toggles `--no-tco`
 - runtimeTypeChecks: Boolean value that toggles `--runtime-type-checks`
 - verboseErrors: Boolean value that toggles `--verbose-errors`
 - browserNamespace: String value that sets `--browser-namespace=<string>`
 - output: String value that sets `--output=<string>`

### purescript.docgen(options)

Invokes the `docgen` command.

#### options

 - hierarchy: Boolean value that toggles `--hierarchy-images`
