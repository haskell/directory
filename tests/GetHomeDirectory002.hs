{-# LANGUAGE CPP #-}
module GetHomeDirectory002 where

#if !defined(mingw32_HOST_OS)
import System.Posix.Env
#endif
#include "util.inl"

-- Test that the getpwuid_r fallback works.
-- This is only relevant on unix.
main :: TestEnv -> IO ()
main _t = do
#if !defined(mingw32_HOST_OS)
  unsetEnv "HOME"
#endif
  _ <- getHomeDirectory
  T(expect) () True -- avoid warnings about redundant imports
