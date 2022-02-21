`directory`
===========

[![Hackage][hi]][hl]
[![Build status][bi]][bl]
[![Dependencies status][di]][dl]

Documentation can be found on [Hackage][hl].
Changes between versions are recorded in the [change log](changelog.md).

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
[bi]: https://github.com/haskell/directory/actions/workflows/build.yml/badge.svg
[bl]: https://github.com/haskell/directory/actions
[di]: https://img.shields.io/hackage-deps/v/directory.svg
[dl]: http://packdeps.haskellers.com/feed?needle=exact:directory
[ac]: https://gnu.org/software/autoconf
