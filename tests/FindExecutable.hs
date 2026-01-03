{-# LANGUAGE CPP #-}
module FindExecutable where
#include "util.inl"

main :: TestEnv -> IO ()
main _t = do

  -- 'find' expected to exist on both Windows and POSIX,
  -- though we have no idea if it's writable
  Just _ <- findExecutable "find"

  T(expectEq) () Nothing =<< findExecutable "__nonexistent_binary_gbowyxcejjawf7r6__"

  -- https://github.com/haskell/directory/issues/187
  T(expectEq) () Nothing =<< findExecutable "/"
  T(expectEq) () Nothing =<< findExecutable "//"
#if !defined(mingw32_HOST_OS)
  T(expectEq) () Nothing =<< findExecutable "\\"
  T(expectEq) () Nothing =<< findExecutable "\\\\"
  T(expectEq) () Nothing =<< findExecutable "\\\\localhost\\c$"
#endif
