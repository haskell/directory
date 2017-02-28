{-# LANGUAGE CPP #-}
-- |
-- Stability: unstable
-- Portability: unportable
--
-- Internal modules are always subject to change from version to version.

#include <HsDirectoryConfig.h>

module System.Directory.Internal
  (

#ifdef mingw32_HOST_OS
    module System.Directory.Internal.Windows
#else
    module System.Directory.Internal.Posix
#endif

#ifdef HAVE_UTIMENSAT
  , module System.Directory.Internal.C_utimensat
#endif

  ) where

#ifdef HAVE_UTIMENSAT
import System.Directory.Internal.C_utimensat
#endif

#ifdef mingw32_HOST_OS
import System.Directory.Internal.Windows
#else
import System.Directory.Internal.Posix
#endif
