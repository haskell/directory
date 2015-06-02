#include <HsDirectoryConfig.h>

module System.Directory.Internal
  ( module System.Directory.Internal

#ifdef HAVE_UTIMENSAT
  , module System.Directory.Internal.C_utimensat
#endif

#ifdef mingw32_HOST_OS
  , module System.Directory.Internal.Windows
#else
  , module System.Directory.Internal.Posix
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

-- | Filename extension for executable files (including the dot if any)
--   (usually @\"\"@ on POSIX systems and @\".exe\"@ on Windows or OS\/2).
exeExtension :: String
exeExtension = (#const_str EXE_EXTENSION)
