{-# LANGUAGE CPP #-}
module CopyFile001 where
#include "util.inl"
import System.Directory
import Data.List (sort)
import Data.Monoid ((<>))
import System.FilePath ((</>))

main :: TestEnv -> IO ()
main _t = do
  createDirectory dir
  writeFile (dir </> from) contents
  T(expectEq) () (specials <> [from]) . sort =<< getDirectoryContents dir
  copyFile (dir </> from) (dir </> to)
  T(expectEq) () (specials <> [from, to]) . sort =<< getDirectoryContents dir
  T(expectEq) () contents =<< readFile (dir </> to)
  where
    specials = [".", ".."]
    contents = "This is the data\n"
    from     = "source"
    to       = "target"
    dir      = "dir"
