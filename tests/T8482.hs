{-# LANGUAGE CPP #-}
module T8482 where
#include "util.inl"
import System.Directory.Internal

tmp1 :: OsPath
tmp1 = "T8482.tmp1"

testdir :: OsPath
testdir = "T8482.dir"

main :: TestEnv -> IO ()
main _t = do
  writeFile (so tmp1) "hello"
  createDirectory testdir
  T(expectIOErrorType) () (is InappropriateType) (renameFile testdir tmp1)
  T(expectIOErrorType) () (is InappropriateType) (renameFile tmp1    testdir)
  T(expectIOErrorType) () (is InappropriateType) (renameFile tmp1    ".")
  where is t = (== t) . ioeGetErrorType
