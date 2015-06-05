{-# LANGUAGE CPP #-}
module CopyFile002 where
#include "util.inl"
import System.Directory
import Data.List (sort)
import Data.Monoid ((<>))

main :: TestEnv -> IO ()
main _t = do
  -- Similar to CopyFile001 but moves a file in the current directory
  -- (Bug #1652 on GHC Trac)
  writeFile from contents
  T(expectEq) () (specials <> [from]) . sort =<< getDirectoryContents "."
  copyFile from to
  T(expectEq) () (specials <> [from, to]) . sort =<< getDirectoryContents "."
  T(expectEq) () contents =<< readFile to
  where
    specials = [".", ".."]
    contents = "This is the data\n"
    from     = "source"
    to       = "target"
