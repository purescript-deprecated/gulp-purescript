'use strict';

var options = {
  psc: {
    cmd: 'psc',
    flags: {
      noPrelude: '--no-prelude',
      noOpts: '--no-opts',
      noMagicDo: '--no-magic-do',
      noTco: '--no-tco',
      main: '--main',
      verboseErrors: '--verbose-errors'
    }
    , single: {browserNamespace: '--browser-namespace', externs: '--externs', main: '--main', output: '--output'}
    , multi: {modules: '--module', codegen: '--codegen'}
  },
  pscMake: {
    cmd: 'psc-make',
    flags: {
      noPrelude: '--no-prelude',
      noOpts: '--no-opts',
      noMagicDo: '--no-magic-do',
      noTco: '--no-tco',
      verboseErrors: '--verbose-errors'
    }
    , single: {browserNamespace: '--browser-namespace', output: '--output'}
    , multi: {}
  },
  pscDocs: {
    cmd: 'psc-docs',
    flags: {
      hierarchy: '--hierarchy-images'
    }
    , single: {}
    , multi: {}
  }
};

module.exports = options;
