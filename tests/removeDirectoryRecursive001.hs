{-# LANGUAGE CPP #-}
module Main (main) where
import Data.List (sort)
import System.Directory
import System.FilePath ((</>), normalise)
import System.IO.Error (catchIOError)
import TestUtils

testName :: String
testName = "removeDirectoryRecursive001"

tmpD :: String
tmpD  = testName ++ ".tmp"

tmp :: String -> String
tmp s = tmpD </> normalise s

main :: IO ()
main = do

  ------------------------------------------------------------
  -- clean up junk from previous invocations

  modifyPermissions (tmp "c") (\ p -> p { writable = True })
    `catchIOError` \ _ -> return ()
  removeDirectoryRecursive tmpD
    `catchIOError` \ _ -> return ()

  ------------------------------------------------------------
  -- set up

  createDirectoryIfMissing True (tmp "a/x/w")
  createDirectoryIfMissing True (tmp "a/y")
  createDirectoryIfMissing True (tmp "a/z")
  createDirectoryIfMissing True (tmp "b")
  createDirectoryIfMissing True (tmp "c")
  writeFile (tmp "a/x/w/u") "foo"
  writeFile (tmp "a/t")     "bar"
  tryCreateSymbolicLink (normalise "../a") (tmp "b/g")
  tryCreateSymbolicLink (normalise "../b") (tmp "c/h")
  tryCreateSymbolicLink (normalise "a")    (tmp "d")
  modifyPermissions (tmp "c") (\ p -> p { writable = False })

  ------------------------------------------------------------
  -- tests

  getDirectoryContents  tmpD     >>= putStrLn . unwords . sort
  getDirectoryContents (tmp "a") >>= putStrLn . unwords . sort
  getDirectoryContents (tmp "b") >>= putStrLn . unwords . sort
  getDirectoryContents (tmp "c") >>= putStrLn . unwords . sort
  getDirectoryContents (tmp "d") >>= putStrLn . unwords . sort

  putStrLn ""

  removeDirectoryRecursive (tmp "d")
    `catchIOError` \ _ -> removeFile      (tmp "d")
#ifdef mingw32_HOST_OS
    `catchIOError` \ _ -> removeDirectory (tmp "d")
#endif

  getDirectoryContents  tmpD     >>= putStrLn . unwords . sort
  getDirectoryContents (tmp "a") >>= putStrLn . unwords . sort
  getDirectoryContents (tmp "b") >>= putStrLn . unwords . sort
  getDirectoryContents (tmp "c") >>= putStrLn . unwords . sort

  putStrLn ""

  removeDirectoryRecursive (tmp "c")
    `catchIOError` \ _ -> do
      modifyPermissions (tmp "c") (\ p -> p { writable = True })
      removeDirectoryRecursive (tmp "c")

  getDirectoryContents  tmpD     >>= putStrLn . unwords . sort
  getDirectoryContents (tmp "a") >>= putStrLn . unwords . sort
  getDirectoryContents (tmp "b") >>= putStrLn . unwords . sort

  putStrLn ""

  removeDirectoryRecursive (tmp "b")

  getDirectoryContents  tmpD     >>= putStrLn . unwords . sort
  getDirectoryContents (tmp "a") >>= putStrLn . unwords . sort

  putStrLn ""

  removeDirectoryRecursive (tmp "a")

  getDirectoryContents  tmpD     >>= putStrLn . unwords . sort

  ------------------------------------------------------------
  -- clean up

  removeDirectoryRecursive tmpD
