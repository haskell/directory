{-# LANGUAGE CPP #-}
module MinimizeNameConflicts
  ( main
  , module System.Directory
#if defined(mingw32_HOST_OS)
  , module System.Win32
#else
  , module System.Posix
#endif
  ) where
#include "util.inl"
#if defined(mingw32_HOST_OS)
import System.Win32 hiding
  ( copyFile
  , createDirectory
  , getCurrentDirectory
  , getTemporaryDirectory
  , removeDirectory
  , setCurrentDirectory
  )
#else
import System.Posix hiding
  ( createDirectory
  , isSymbolicLink
  , removeDirectory
  )
#endif

-- This is just a compile-test to check for name conflicts between directory
-- and other boot libraries. See for example:
-- https://github.com/haskell/directory/issues/52
main :: TestEnv -> IO ()
main _t = do
  T(expect) "no-op" True
