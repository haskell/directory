# Changelog for [`directory` package](http://hackage.haskell.org/package/directory)

## 1.2.2.0  *Jan 2014*

  * Bundled with GHC 7.10.1

  * make `getModificationTime` support sub-second resolution on windows

  * Fix silent failure in `createDirectoryIfMissing`

  * Replace `throw` by better defined `throwIO`s

  * Avoid stack overflow in `getDirectoryContents` [#17](https://github.com/haskell/directory/pull/17)

  * Expose `findExecutables` [#14](https://github.com/haskell/directory/issues/14)

  * Clarify conditions under which `removeDirectoryRecursive` may follow a symlink

## 1.2.1.0  *Mar 2014*

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
