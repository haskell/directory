`directory` [![Hackage][1]][2] [![Build Status][3]][4]
===========

Documentation can be found on [Hackage][2].

Building from Git repository
----------------------------

When building this package directly from the Git repository, one must run
`autoreconf -i` to generate the `configure` script needed by `cabal
configure`.  This requires [Autoconf][5] to be installed.

    autoreconf -i
    cabal install

There is no need to run the `configure` script manually however, as `cabal
configure` does that automatically.

[1]: https://img.shields.io/hackage/v/directory.svg
[2]: https://hackage.haskell.org/package/directory
[3]: https://travis-ci.org/haskell/directory.svg?branch=master
[4]: https://travis-ci.org/haskell/directory
[5]: https://gnu.org/software/autoconf
