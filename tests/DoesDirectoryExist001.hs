{-# LANGUAGE CPP #-}
module DoesDirectoryExist001 where
#include "util.inl"
import System.Directory

main :: TestEnv -> IO ()
main _t = do

  -- [regression test] "/" was not recognised as a directory prior to GHC 6.1
  T(expect) () =<< doesDirectoryExist rootDir

  where
#ifdef mingw32_HOST_OS
    rootDir = "C:\\"
#else
    rootDir = "/"
#endif
