{-# LANGUAGE CPP #-}
module GetHomeDirectory002 where

#if !defined(mingw32_HOST_OS)
import System.Posix.Env
#endif
import Prelude ()
import System.Directory.Internal.Prelude
import System.Directory.OsPath
import TestUtils ()
import Util (TestEnv)
import qualified Util as T

-- Test that the getpwuid_r fallback works.
-- This is only relevant on unix.
main :: TestEnv -> IO ()
main _t = do
#if !defined(mingw32_HOST_OS)
  unsetEnv "HOME"
#endif
  _ <- getHomeDirectory
  T.expect _t () True -- avoid warnings about redundant imports
