{-# LANGUAGE CPP #-}
module DoesPathExist where
#include "util.inl"
import TestUtils (supportsSymlinks)

main :: TestEnv -> IO ()
main _t = do

  T(expect) () =<< doesPathExist rootDir

  createDirectory "somedir"
  writeFile "somefile" "somedata"
  writeFile "\x3c0\x42f\x97f3\xe6\x221e" "somedata"

  T(expect) () . not =<< doesPathExist ""
  T(expect) () . not =<< doesPathExist "nonexistent"
  T(expect) () =<< doesPathExist "."
  T(expect) () =<< doesPathExist "somedir"
  T(expect) () =<< doesPathExist "somefile"
  T(expect) () =<< doesPathExist "./somefile"
#if defined(mingw32_HOST_OS)
  T(expect) () =<< doesPathExist "SoMeDiR"
  T(expect) () =<< doesPathExist "sOmEfIlE"
#endif
  T(expect) () =<< doesPathExist "\x3c0\x42f\x97f3\xe6\x221e"

  supportsSymbolicLinks <- supportsSymlinks
  when supportsSymbolicLinks $ do

    createDirectoryLink "somedir" "somedirlink"
    createFileLink "somefile" "somefilelink"
    createFileLink "nonexistent" "nonexistentlink"

    T(expect) () =<< doesFileExist "somefilelink"
    T(expect) () . not =<< doesDirectoryExist "somefilelink"
    T(expect) () =<< doesDirectoryExist "somedirlink"
    T(expect) () . not =<< doesFileExist "somedirlink"
    T(expect) () . not =<< doesDirectoryExist "nonexistentlink"
    T(expect) () . not =<< doesFileExist "nonexistentlink"

  where
#if defined(mingw32_HOST_OS)
    rootDir = "C:\\"
#else
    rootDir = "/"
#endif
