{-# LANGUAGE CPP #-}
module CopyFile001 where
#include "util.inl"
import System.Directory
import Data.List (sort)
import System.FilePath ((</>))

main :: TestEnv -> IO ()
main _t = do
  createDirectory dir
  writeFile (dir </> from) contents
  T(expectEq) () [from] . sort =<< getDirectoryContentsA dir
  copyFile (dir </> from) (dir </> to)
  T(expectEq) () [from, to] . sort =<< getDirectoryContentsA dir
  T(expectEq) () contents =<< readFile (dir </> to)
  where
    contents = "This is the data\n"
    from     = "source"
    to       = "target"
    dir      = "dir"
