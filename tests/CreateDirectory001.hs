{-# LANGUAGE CPP #-}
module CreateDirectory001 where
#include "util.inl"
import System.Directory
import System.IO.Error (isAlreadyExistsError)

main :: TestEnv -> IO ()
main _t = do
  createDirectory testdir
  T(expectIOErrorType) () isAlreadyExistsError (createDirectory testdir)
  where testdir = "dir"
