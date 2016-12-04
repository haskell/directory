module System.Directory.Internal.Windows where
#include <HsDirectoryConfig.h>
#ifdef mingw32_HOST_OS
#include <shlobj.h>
#include <windows.h>
#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif
import Prelude ()
import System.Directory.Internal.Prelude
import qualified System.Win32 as Win32

win32_cSIDL_LOCAL_APPDATA :: Win32.CSIDL
#if MIN_VERSION_Win32(2, 3, 1)
win32_cSIDL_LOCAL_APPDATA = Win32.cSIDL_LOCAL_APPDATA
#else
win32_cSIDL_LOCAL_APPDATA = (#const CSIDL_LOCAL_APPDATA)
#endif

win32_fILE_ATTRIBUTE_REPARSE_POINT :: Win32.FileAttributeOrFlag
win32_fILE_ATTRIBUTE_REPARSE_POINT = (#const FILE_ATTRIBUTE_REPARSE_POINT)

win32_fILE_SHARE_DELETE :: Win32.ShareMode
#if MIN_VERSION_Win32(2, 3, 1)
win32_fILE_SHARE_DELETE = Win32.fILE_SHARE_DELETE -- added in 2.3.0.2
#else
win32_fILE_SHARE_DELETE = (#const FILE_SHARE_DELETE)
#endif

foreign import ccall unsafe "_wchmod"
  c_wchmod :: CWString -> CMode -> IO CInt

s_IRUSR :: CMode
s_IRUSR = (#const S_IRUSR)

s_IWUSR :: CMode
s_IWUSR = (#const S_IWUSR)

s_IXUSR :: CMode
s_IXUSR = (#const S_IXUSR)

s_IFDIR :: CMode
s_IFDIR = (#const S_IFDIR)

#endif
