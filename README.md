The `unix` Package  [![Build Status](https://travis-ci.org/ghc/packages-unix.png?branch=master)](https://travis-ci.org/ghc/packages-unix)
==================

See [`unix` on Hackage](http://hackage.haskell.org/package/unix) for
more information.

Installing from Git
-------------------

To build this package using Cabal directly from Git, you must run
`autoreconf -i` before the usual Cabal build steps (`cabal
{configure,build,install}`). The program `autoreconf` is part of
[GNU autoconf](http://www.gnu.org/software/autoconf/).  There is no
need to run the `configure` script: `cabal configure` will do this for
you.
