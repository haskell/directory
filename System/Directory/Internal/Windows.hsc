{-# LANGUAGE CPP #-}
module System.Directory.Internal.Windows where
#include <HsDirectoryConfig.h>
#ifdef mingw32_HOST_OS
##if defined i386_HOST_ARCH
## define WINAPI stdcall
##elif defined x86_64_HOST_ARCH
## define WINAPI ccall
##else
## error unknown architecture
##endif
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

win32_getLongPathName, win32_getShortPathName :: FilePath -> IO FilePath
#if MIN_VERSION_Win32(2, 4, 0)
win32_getLongPathName = Win32.getLongPathName
win32_getShortPathName = Win32.getShortPathName
#else
win32_getLongPathName path =
  modifyIOError ((`ioeSetLocation` "GetLongPathName") .
                 (`ioeSetFileName` path)) $ do
    withCWString path $ \ ptrPath -> do
      getPathNameWith (c_GetLongPathName ptrPath)

win32_getShortPathName path =
  modifyIOError ((`ioeSetLocation` "GetShortPathName") .
                 (`ioeSetFileName` path)) $ do
    withCWString path $ \ ptrPath -> do
      getPathNameWith (c_GetShortPathName ptrPath)

foreign import WINAPI unsafe "windows.h GetLongPathNameW"
  c_GetLongPathName
    :: Ptr CWchar
    -> Ptr CWchar
    -> Win32.DWORD
    -> IO Win32.DWORD

foreign import WINAPI unsafe "windows.h GetShortPathNameW"
  c_GetShortPathName
    :: Ptr CWchar
    -> Ptr CWchar
    -> Win32.DWORD
    -> IO Win32.DWORD
#endif

getPathNameWith :: (Ptr CWchar -> Win32.DWORD -> IO Win32.DWORD) -> IO FilePath
getPathNameWith cFunc = do
  let getPathNameWithLen len = do
        allocaArray (fromIntegral len) $ \ ptrPathOut -> do
          len' <- Win32.failIfZero "" (cFunc ptrPathOut len)
          if len' <= len
            then Right <$> peekCWStringLen (ptrPathOut, fromIntegral len')
            else pure (Left len')
  r <- getPathNameWithLen ((#const MAX_PATH) * (#size wchar_t))
  case r of
    Right s -> pure s
    Left len -> do
      r' <- getPathNameWithLen len
      case r' of
        Right s -> pure s
        Left _ -> ioError (mkIOError OtherError "" Nothing Nothing
                           `ioeSetErrorString` "path changed unexpectedly")

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
