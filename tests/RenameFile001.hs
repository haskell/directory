{-# LANGUAGE CPP #-}
module RenameFile001 where
#include "util.inl"
import System.Directory.Internal

main :: TestEnv -> IO ()
main _t = do
  writeFile tmp1 contents1
  renameFile (os tmp1) (os tmp2)
  T(expectEq) () contents1 =<< readFile tmp2
  writeFile tmp1 contents2
  renameFile (os tmp2) (os tmp1)
  T(expectEq) () contents1 =<< readFile tmp1
  where
    tmp1 = "tmp1"
    tmp2 = "tmp2"
    contents1 = "test"
    contents2 = "test2"
