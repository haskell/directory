{-# LANGUAGE CPP #-}
module RemovePathForcibly where
#include "util.inl"
import System.Directory.Internal
import System.OsPath ((</>), normalise)
import qualified Data.List as List
import TestUtils (hardLinkOrCopy, modifyPermissions, symlinkOrCopy)

main :: TestEnv -> IO ()
main _t = do

  ------------------------------------------------------------
  -- clean up junk from previous invocations

  modifyPermissions (tmp "c") (\ p -> p { writable = True })
    `catchIOError` \ _ -> return ()
  removePathForcibly tmpD
    `catchIOError` \ _ -> return ()

  ------------------------------------------------------------
  -- set up

  createDirectoryIfMissing True (tmp "a/x/w")
  createDirectoryIfMissing True (tmp "a/y")
  createDirectoryIfMissing True (tmp "a/z")
  createDirectoryIfMissing True (tmp "b")
  createDirectoryIfMissing True (tmp "c")
  createDirectoryIfMissing True (tmp "f")
  writeFile (so (tmp "a/x/w/u")) "foo"
  writeFile (so (tmp "a/t"))     "bar"
  writeFile (so (tmp "f/s"))     "qux"
  symlinkOrCopy (normalise "../a") (tmp "b/g")
  symlinkOrCopy (normalise "../b") (tmp "c/h")
  symlinkOrCopy (normalise "a")    (tmp "d")
  setPermissions (tmp "f/s") emptyPermissions
  setPermissions (tmp "f") emptyPermissions

  ------------------------------------------------------------
  -- tests

  removePathForcibly (tmp "f")
  removePathForcibly (tmp "e") -- intentionally non-existent

  T(expectEq) () [".", "..", "a", "b", "c", "d"] . List.sort =<<
    getDirectoryContents  tmpD
  T(expectEq) () [".", "..", "t", "x", "y", "z"] . List.sort =<<
    getDirectoryContents (tmp "a")
  T(expectEq) () [".", "..", "g"] . List.sort =<<
    getDirectoryContents (tmp "b")
  T(expectEq) () [".", "..", "h"] . List.sort =<<
    getDirectoryContents (tmp "c")
  T(expectEq) () [".", "..", "t", "x", "y", "z"] . List.sort =<<
    getDirectoryContents (tmp "d")

  removePathForcibly (tmp "d")

  T(expectEq) () [".", "..", "a", "b", "c"] . List.sort =<<
    getDirectoryContents  tmpD
  T(expectEq) () [".", "..", "t", "x", "y", "z"] . List.sort =<<
    getDirectoryContents (tmp "a")
  T(expectEq) () [".", "..", "g"] . List.sort =<<
    getDirectoryContents (tmp "b")
  T(expectEq) () [".", "..", "h"] . List.sort =<<
    getDirectoryContents (tmp "c")

  removePathForcibly (tmp "c")

  T(expectEq) () [".", "..", "a", "b"] . List.sort =<<
    getDirectoryContents  tmpD
  T(expectEq) () [".", "..", "t", "x", "y", "z"] . List.sort =<<
   getDirectoryContents (tmp "a")
  T(expectEq) () [".", "..", "g"] . List.sort =<<
    getDirectoryContents (tmp "b")

  removePathForcibly (tmp "b")

  T(expectEq) () [".", "..", "a"] . List.sort =<<
    getDirectoryContents  tmpD
  T(expectEq) () [".", "..", "t", "x", "y", "z"] . List.sort =<<
    getDirectoryContents (tmp "a")

  removePathForcibly (tmp "a")

  T(expectEq) () [".", ".."] . List.sort =<<
    getDirectoryContents  tmpD

  ----------------------------------------------------------------------
  -- regression test for https://github.com/haskell/directory/issues/135
  
  writeFile "hl1" "hardlinked"
  setPermissions "hl1" emptyPermissions
  origPermissions <- getPermissions "hl1"
  hardLinkOrCopy "hl1" "hl2"
  removePathForcibly "hl2"
  T(expectEq) () origPermissions =<< getPermissions "hl1"

  where testName = "removePathForcibly"
        tmpD  = testName <> ".tmp"
        tmp s = tmpD </> normalise s
