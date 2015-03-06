Changelog for the [`directory`][1] package
==========================================

## 1.2.2.0 (Mar 2015)

  * Bundled with GHC 7.10.1

  * Make `getModificationTime` support sub-second resolution on Windows

  * Fix silent failure in `createDirectoryIfMissing`

  * Replace `throw` by better defined `throwIO`s

  * [#17](https://github.com/haskell/directory/pull/17):
    Avoid stack overflow in `getDirectoryContents`

  * [#14](https://github.com/haskell/directory/issues/14):
    Expose `findExecutables`

  * [#15](https://github.com/haskell/directory/issues/15):
    `removeDirectoryRecursive` no longer follows symlinks under any
    circumstances

  * [#9](https://github.com/haskell/directory/issues/9):
    Allow trailing path separators in `getPermissions` on Windows

  * [#8](https://github.com/haskell/directory/pull/8):
    `renameFile` now always throws the correct error type
    (`InappropriateType`) when the destination is a directory, as long as the
    filesystem is not being modified concurrently

  * Add `makeAbsolute`, which should be preferred over `canonicalizePath`
    unless one requires symbolic links to be resolved

## 1.2.1.0 (Mar 2014)

  * Bundled with GHC 7.8.1

  * Add support for sub-second precision in `getModificationTime` when
    linked against `unix>=2.6.0.0`

  * Fix `createDirectoryIfMissing _ "."` in `C:\` on Windows

  * Remove support for NHC98 compiler

  * Update package to `cabal-version >= 1.10` format

  * Enhance Haddock documentation for `doesDirectoryExist` and
    `canonicalizePath`

  * Fix `findExecutable` to check that file permissions indicate executable

  * New convenience functions `findFiles` and `findFilesWith`

[1]: https://hackage.haskell.org/package/directory
