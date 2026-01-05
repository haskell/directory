{-# LANGUAGE CPP #-}
module FindExecutable where
import Prelude ()
import System.Directory.Internal.Prelude
import System.Directory.OsPath
import TestUtils ()
import Util (TestEnv)
import qualified Util as T

main :: TestEnv -> IO ()
main _t = do

  -- 'find' expected to exist on both Windows and POSIX,
  -- though we have no idea if it's writable
  Just _ <- findExecutable "find"

  T.expectEq _t () Nothing =<<
    findExecutable "__nonexistent_binary_gbowyxcejjawf7r6__"

  -- https://github.com/haskell/directory/issues/187
  T.expectEq _t () Nothing =<< findExecutable "/"
  T.expectEq _t () Nothing =<< findExecutable "//"
#if !defined(mingw32_HOST_OS)
  T.expectEq _t () Nothing =<< findExecutable "\\"
  T.expectEq _t () Nothing =<< findExecutable "\\\\"
  T.expectEq _t () Nothing =<< findExecutable "\\\\localhost\\c$"
#endif
