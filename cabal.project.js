packages: .

with-compiler: javascript-unknown-ghcjs-ghc
with-hc-pkg: javascript-unknown-ghcjs-ghc-pkg

package unix
  ghc-options: -Wno-unused-imports

write-ghc-environment-files: always

allow-newer: all:base

tests: True

constraints:
  tasty -unix, optparse-applicative -process,
  unix -os-string
