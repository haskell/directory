{-# LANGUAGE CPP #-}
module GetDirContents002 where
#include "util.inl"
import System.Directory
import System.IO.Error (isDoesNotExistError)

main :: TestEnv -> IO ()
main _t = do
  T(expectIOErrorType) () isDoesNotExistError $
    getDirectoryContents "nonexistent"
