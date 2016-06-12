`directory`
===========

[![Hackage][hi]][hl]
[![Build status][bi]][bl]
[![Windows build status][wi]][wl]

Documentation can be found on [Hackage][hl].

Building from Git repository
----------------------------

When building this package directly from the Git repository, one must run
`autoreconf -fi` to generate the `configure` script needed by `cabal
configure`.  This requires [Autoconf][ac] to be installed.

    autoreconf -fi
    cabal install

There is no need to run the `configure` script manually however, as `cabal
configure` does that automatically.

[hi]: https://img.shields.io/hackage/v/directory.svg
[hl]: https://hackage.haskell.org/package/directory
[bi]: https://travis-ci.org/haskell/directory.svg?branch=master
[bl]: https://travis-ci.org/haskell/directory
[wi]: https://ci.appveyor.com/api/projects/status/github/haskell/directory?branch=master&svg=true
[wl]: https://ci.appveyor.com/project/hvr/directory
[ac]: https://gnu.org/software/autoconf
