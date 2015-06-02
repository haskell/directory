#include <HsDirectory.h>

#ifndef mingw32_HOST_OS
# include <HsUnixConfig.h>
#endif

module System.Directory.Internal
  ( module System.Directory.Internal

#ifdef HAVE_UTIMENSAT
  , module System.Directory.Internal.C_utimensat
#endif

) where

#ifdef HAVE_UTIMENSAT
import System.Directory.Internal.C_utimensat
#endif

-- | Filename extension for executable files (including the dot if any)
--   (usually @\"\"@ on POSIX systems and @\".exe\"@ on Windows or OS\/2).
exeExtension :: String
exeExtension = (#const_str EXE_EXTENSION)
