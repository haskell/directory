{-# LANGUAGE CPP #-}
module CurrentDirectory001 where
#include "util.inl"
import System.Directory
import Data.List (sort)

main :: TestEnv -> IO ()
main _t = do
  prevDir <- getCurrentDirectory
  createDirectory "dir"
  setCurrentDirectory "dir"
  T(expectEq) () [".", ".."] . sort =<< getDirectoryContents "."
  setCurrentDirectory prevDir
  removeDirectory "dir"
