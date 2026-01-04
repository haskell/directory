{-# LANGUAGE CPP #-}
module DoesDirectoryExist001 where
import Prelude ()
import System.Directory.Internal.Prelude
import System.Directory.OsPath
import TestUtils ()
import Util (TestEnv)
import qualified Util as T

main :: TestEnv -> IO ()
main _t = do

  -- [regression test] "/" was not recognised as a directory prior to GHC 6.1
  T.expect _t () =<< doesDirectoryExist rootDir

  createDirectory "somedir"

  T.expect _t () . not =<< doesDirectoryExist ""
  T.expect _t () . not =<< doesDirectoryExist "nonexistent"
  T.expect _t () =<< doesDirectoryExist "."
  T.expect _t () =<< doesDirectoryExist "somedir"
#if defined(mingw32_HOST_OS)
  T.expect _t () =<< doesDirectoryExist "SoMeDiR"
#endif

  where
#if defined(mingw32_HOST_OS)
    rootDir = "C:\\"
#else
    rootDir = "/"
#endif
