{-# LANGUAGE CPP #-}
module RemoveDirectoryRecursive001 where
import Prelude ()
import System.Directory.Internal.Prelude
import System.Directory.Internal
import System.Directory.OsPath
import TestUtils (modifyPermissions, symlinkOrCopy)
import Util (TestEnv)
import qualified Util as T
import System.OsPath ((</>), normalise)
import qualified Data.List as List

main :: TestEnv -> IO ()
main _t = do

  ------------------------------------------------------------
  -- clean up junk from previous invocations

  modifyPermissions (tmp "c") (\ p -> p { writable = True })
    `catchIOError` \ _ -> pure ()
  removeDirectoryRecursive tmpD
    `catchIOError` \ _ -> pure ()

  ------------------------------------------------------------
  -- set up

  createDirectoryIfMissing True (tmp "a/x/w")
  createDirectoryIfMissing True (tmp "a/y")
  createDirectoryIfMissing True (tmp "a/z")
  createDirectoryIfMissing True (tmp "b")
  createDirectoryIfMissing True (tmp "c")
  writeFile (so (tmp "a/x/w/u")) "foo"
  writeFile (so (tmp "a/t"))     "bar"
  symlinkOrCopy (normalise "../a") (tmp "b/g")
  symlinkOrCopy (normalise "../b") (tmp "c/h")
  symlinkOrCopy (normalise "a")    (tmp "d")
  modifyPermissions (tmp "c") (\ p -> p { writable = False })

  ------------------------------------------------------------
  -- tests

  T.expectEq _t () [".", "..", "a", "b", "c", "d"] . List.sort =<<
    getDirectoryContents  tmpD
  T.expectEq _t () [".", "..", "t", "x", "y", "z"] . List.sort =<<
    getDirectoryContents (tmp "a")
  T.expectEq _t () [".", "..", "g"] . List.sort =<<
    getDirectoryContents (tmp "b")
  T.expectEq _t () [".", "..", "h"] . List.sort =<<
    getDirectoryContents (tmp "c")
  T.expectEq _t () [".", "..", "t", "x", "y", "z"] . List.sort =<<
    getDirectoryContents (tmp "d")

  removeDirectoryRecursive (tmp "d")
    `catchIOError` \ _ -> removeFile      (tmp "d")
#if defined(mingw32_HOST_OS)
    `catchIOError` \ _ -> removeDirectory (tmp "d")
#endif

  T.expectEq _t () [".", "..", "a", "b", "c"] . List.sort =<<
    getDirectoryContents  tmpD
  T.expectEq _t () [".", "..", "t", "x", "y", "z"] . List.sort =<<
    getDirectoryContents (tmp "a")
  T.expectEq _t () [".", "..", "g"] . List.sort =<<
    getDirectoryContents (tmp "b")
  T.expectEq _t () [".", "..", "h"] . List.sort =<<
    getDirectoryContents (tmp "c")

  removeDirectoryRecursive (tmp "c")
    `catchIOError` \ _ -> do
      modifyPermissions (tmp "c") (\ p -> p { writable = True })
      removeDirectoryRecursive (tmp "c")

  T.expectEq _t () [".", "..", "a", "b"] . List.sort =<<
    getDirectoryContents  tmpD
  T.expectEq _t () [".", "..", "t", "x", "y", "z"] . List.sort =<<
   getDirectoryContents (tmp "a")
  T.expectEq _t () [".", "..", "g"] . List.sort =<<
    getDirectoryContents (tmp "b")

  removeDirectoryRecursive (tmp "b")

  T.expectEq _t () [".", "..", "a"] . List.sort =<<
    getDirectoryContents  tmpD
  T.expectEq _t () [".", "..", "t", "x", "y", "z"] . List.sort =<<
    getDirectoryContents (tmp "a")

  removeDirectoryRecursive (tmp "a")

  T.expectEq _t () [".", ".."] . List.sort =<<
    getDirectoryContents  tmpD

  where testName = "removeDirectoryRecursive001"
        tmpD  = testName <> ".tmp"
        tmp s = tmpD </> normalise s
