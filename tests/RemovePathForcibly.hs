{-# LANGUAGE CPP #-}
module RemovePathForcibly where
#include "util.inl"
import System.Directory
import Data.List (sort)
import System.FilePath ((</>), normalise)
import System.IO.Error (catchIOError)
import TestUtils

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
  writeFile (tmp "a/x/w/u") "foo"
  writeFile (tmp "a/t")     "bar"
  writeFile (tmp "f/s")     "qux"
  tryCreateSymbolicLink (normalise "../a") (tmp "b/g")
  tryCreateSymbolicLink (normalise "../b") (tmp "c/h")
  tryCreateSymbolicLink (normalise "a")    (tmp "d")
  setPermissions (tmp "f/s") emptyPermissions
  setPermissions (tmp "f") emptyPermissions

  ------------------------------------------------------------
  -- tests

  removePathForcibly (tmp "f")
  removePathForcibly (tmp "e") -- intentionally non-existent

  T(expectEq) () [".", "..", "a", "b", "c", "d"] . sort =<<
    getDirectoryContents  tmpD
  T(expectEq) () [".", "..", "t", "x", "y", "z"] . sort =<<
    getDirectoryContents (tmp "a")
  T(expectEq) () [".", "..", "g"] . sort =<<
    getDirectoryContents (tmp "b")
  T(expectEq) () [".", "..", "h"] . sort =<<
    getDirectoryContents (tmp "c")
  T(expectEq) () [".", "..", "t", "x", "y", "z"] . sort =<<
    getDirectoryContents (tmp "d")

  removePathForcibly (tmp "d")

  T(expectEq) () [".", "..", "a", "b", "c"] . sort =<<
    getDirectoryContents  tmpD
  T(expectEq) () [".", "..", "t", "x", "y", "z"] . sort =<<
    getDirectoryContents (tmp "a")
  T(expectEq) () [".", "..", "g"] . sort =<<
    getDirectoryContents (tmp "b")
  T(expectEq) () [".", "..", "h"] . sort =<<
    getDirectoryContents (tmp "c")

  removePathForcibly (tmp "c")

  T(expectEq) () [".", "..", "a", "b"] . sort =<<
    getDirectoryContents  tmpD
  T(expectEq) () [".", "..", "t", "x", "y", "z"] . sort =<<
   getDirectoryContents (tmp "a")
  T(expectEq) () [".", "..", "g"] . sort =<<
    getDirectoryContents (tmp "b")

  removePathForcibly (tmp "b")

  T(expectEq) () [".", "..", "a"] . sort =<<
    getDirectoryContents  tmpD
  T(expectEq) () [".", "..", "t", "x", "y", "z"] . sort =<<
    getDirectoryContents (tmp "a")

  removePathForcibly (tmp "a")

  T(expectEq) () [".", ".."] . sort =<<
    getDirectoryContents  tmpD

  where testName = "removePathForcibly"
        tmpD  = testName ++ ".tmp"
        tmp s = tmpD </> normalise s
