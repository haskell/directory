{-# LANGUAGE CPP #-}
module DoesPathExist where
import Prelude ()
import System.Directory.Internal.Prelude
import System.Directory.OsPath
import TestUtils (supportsSymlinks)
import Util (TestEnv)
import qualified Util as T

main :: TestEnv -> IO ()
main _t = do

  T.expect _t () =<< doesPathExist rootDir

  createDirectory "somedir"
  writeFile "somefile" "somedata"
  writeFile "\x3c0\x42f\x97f3\xe6\x221e" "somedata"

  T.expect _t () . not =<< doesPathExist ""
  T.expect _t () . not =<< doesPathExist "nonexistent"
  T.expect _t () =<< doesPathExist "."
  T.expect _t () =<< doesPathExist "somedir"
  T.expect _t () =<< doesPathExist "somefile"
  T.expect _t () =<< doesPathExist "./somefile"
#if defined(mingw32_HOST_OS)
  T.expect _t () =<< doesPathExist "SoMeDiR"
  T.expect _t () =<< doesPathExist "sOmEfIlE"
#endif
  T.expect _t () =<< doesPathExist "\x3c0\x42f\x97f3\xe6\x221e"

  supportsSymbolicLinks <- supportsSymlinks
  when supportsSymbolicLinks $ do

    createDirectoryLink "somedir" "somedirlink"
    createFileLink "somefile" "somefilelink"
    createFileLink "nonexistent" "nonexistentlink"

    T.expect _t () =<< doesFileExist "somefilelink"
    T.expect _t () . not =<< doesDirectoryExist "somefilelink"
    T.expect _t () =<< doesDirectoryExist "somedirlink"
    T.expect _t () . not =<< doesFileExist "somedirlink"
    T.expect _t () . not =<< doesDirectoryExist "nonexistentlink"
    T.expect _t () . not =<< doesFileExist "nonexistentlink"

  where
#if defined(mingw32_HOST_OS)
    rootDir = "C:\\"
#else
    rootDir = "/"
#endif
