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
#include <System/Directory/Internal/utility.h>
#include <System/Directory/Internal/windows.h>
import Prelude ()
import System.Directory.Internal.Prelude
import System.FilePath (addTrailingPathSeparator, hasTrailingPathSeparator,
                        isPathSeparator, isRelative, joinDrive, joinPath,
                        normalise, pathSeparator, pathSeparators,
                        splitDirectories, splitDrive)
import qualified Data.List as List
import qualified System.Win32 as Win32

win32_cSIDL_LOCAL_APPDATA :: Win32.CSIDL
#if MIN_VERSION_Win32(2, 3, 1)
win32_cSIDL_LOCAL_APPDATA = Win32.cSIDL_LOCAL_APPDATA
#else
win32_cSIDL_LOCAL_APPDATA = (#const CSIDL_LOCAL_APPDATA)
#endif

win32_eRROR_INVALID_FUNCTION :: Win32.ErrCode
win32_eRROR_INVALID_FUNCTION = 0x1

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

win32_getFinalPathNameByHandle :: Win32.HANDLE -> Win32.DWORD -> IO FilePath
win32_getFinalPathNameByHandle _h _flags =
  modifyIOError (`ioeSetLocation` "GetFinalPathNameByHandle") $ do
#ifdef HAVE_GETFINALPATHNAMEBYHANDLEW
    getPathNameWith $ \ ptr len -> do
      c_GetFinalPathNameByHandle _h ptr len _flags

foreign import WINAPI unsafe "windows.h GetFinalPathNameByHandleW"
  c_GetFinalPathNameByHandle
    :: Win32.HANDLE
    -> Ptr CWchar
    -> Win32.DWORD
    -> Win32.DWORD
    -> IO Win32.DWORD

#else
    throwIO (mkIOError UnsupportedOperation
             "platform does not support GetFinalPathNameByHandle"
             Nothing Nothing)
#endif

getFinalPathName :: FilePath -> IO FilePath
getFinalPathName =
  (fromExtendedLengthPath <$>) . rawGetFinalPathName . toExtendedLengthPath
  where
#ifdef HAVE_GETFINALPATHNAMEBYHANDLEW
    rawGetFinalPathName path = do
      let open = Win32.createFile path 0 shareMode Nothing
                 Win32.oPEN_EXISTING Win32.fILE_FLAG_BACKUP_SEMANTICS Nothing
      bracket open Win32.closeHandle $ \ h -> do
        win32_getFinalPathNameByHandle h 0
    shareMode =
      win32_fILE_SHARE_DELETE .|.
      Win32.fILE_SHARE_READ   .|.
      Win32.fILE_SHARE_WRITE
#else
    rawGetFinalPathName = win32_getLongPathName <=< win32_getShortPathName
#endif

win32_fILE_FLAG_OPEN_REPARSE_POINT :: Win32.FileAttributeOrFlag
win32_fILE_FLAG_OPEN_REPARSE_POINT = 0x00200000

win32_fSCTL_GET_REPARSE_POINT :: Win32.DWORD
win32_fSCTL_GET_REPARSE_POINT = 0x900a8

win32_iO_REPARSE_TAG_MOUNT_POINT, win32_iO_REPARSE_TAG_SYMLINK :: CULong
win32_iO_REPARSE_TAG_MOUNT_POINT = (#const IO_REPARSE_TAG_MOUNT_POINT)
win32_iO_REPARSE_TAG_SYMLINK = (#const IO_REPARSE_TAG_SYMLINK)

win32_mAXIMUM_REPARSE_DATA_BUFFER_SIZE :: Win32.DWORD
win32_mAXIMUM_REPARSE_DATA_BUFFER_SIZE =
  (#const MAXIMUM_REPARSE_DATA_BUFFER_SIZE)

win32_sYMLINK_FLAG_RELATIVE :: CULong
win32_sYMLINK_FLAG_RELATIVE = 0x00000001

data Win32_REPARSE_DATA_BUFFER
  = Win32_MOUNT_POINT_REPARSE_DATA_BUFFER String String
    -- ^ substituteName printName
  | Win32_SYMLINK_REPARSE_DATA_BUFFER String String Bool
    -- ^ substituteName printName isRelative
  | Win32_GENERIC_REPARSE_DATA_BUFFER

win32_alloca_REPARSE_DATA_BUFFER
  :: ((Ptr Win32_REPARSE_DATA_BUFFER, Int) -> IO a) -> IO a
win32_alloca_REPARSE_DATA_BUFFER action =
  allocaBytesAligned size align $ \ ptr ->
    action (ptr, size)
  where size = fromIntegral win32_mAXIMUM_REPARSE_DATA_BUFFER_SIZE
        -- workaround (hsc2hs for GHC < 8.0 don't support #{alignment ...})
        align = #{size char[alignof(HsDirectory_REPARSE_DATA_BUFFER)]}

win32_peek_REPARSE_DATA_BUFFER
  :: Ptr Win32_REPARSE_DATA_BUFFER -> IO Win32_REPARSE_DATA_BUFFER
win32_peek_REPARSE_DATA_BUFFER p = do
  tag <- #{peek HsDirectory_REPARSE_DATA_BUFFER, ReparseTag} p
  case () of
    _ | tag == win32_iO_REPARSE_TAG_MOUNT_POINT -> do
          let buf = #{ptr HsDirectory_REPARSE_DATA_BUFFER,
                          MountPointReparseBuffer.PathBuffer} p
          sni <- #{peek HsDirectory_REPARSE_DATA_BUFFER,
                        MountPointReparseBuffer.SubstituteNameOffset} p
          sns <- #{peek HsDirectory_REPARSE_DATA_BUFFER,
                        MountPointReparseBuffer.SubstituteNameLength} p
          sn <- peekName buf sni sns
          pni <- #{peek HsDirectory_REPARSE_DATA_BUFFER,
                        MountPointReparseBuffer.PrintNameOffset} p
          pns <- #{peek HsDirectory_REPARSE_DATA_BUFFER,
                        MountPointReparseBuffer.PrintNameLength} p
          pn <- peekName buf pni pns
          pure (Win32_MOUNT_POINT_REPARSE_DATA_BUFFER sn pn)
      | tag == win32_iO_REPARSE_TAG_SYMLINK -> do
          let buf = #{ptr HsDirectory_REPARSE_DATA_BUFFER,
                          SymbolicLinkReparseBuffer.PathBuffer} p
          sni <- #{peek HsDirectory_REPARSE_DATA_BUFFER,
                        SymbolicLinkReparseBuffer.SubstituteNameOffset} p
          sns <- #{peek HsDirectory_REPARSE_DATA_BUFFER,
                        SymbolicLinkReparseBuffer.SubstituteNameLength} p
          sn <- peekName buf sni sns
          pni <- #{peek HsDirectory_REPARSE_DATA_BUFFER,
                        SymbolicLinkReparseBuffer.PrintNameOffset} p
          pns <- #{peek HsDirectory_REPARSE_DATA_BUFFER,
                        SymbolicLinkReparseBuffer.PrintNameLength} p
          pn <- peekName buf pni pns
          flags <- #{peek HsDirectory_REPARSE_DATA_BUFFER,
                          SymbolicLinkReparseBuffer.Flags} p
          pure (Win32_SYMLINK_REPARSE_DATA_BUFFER sn pn
                (flags .&. win32_sYMLINK_FLAG_RELATIVE /= 0))
      | otherwise -> pure Win32_GENERIC_REPARSE_DATA_BUFFER
  where
    peekName :: Ptr CWchar -> CUShort -> CUShort -> IO String
    peekName buf offset size =
      peekCWStringLen ( buf `plusPtr` fromIntegral offset
                      , fromIntegral size `div` sizeOf (0 :: CWchar) )

deviceIoControl
  :: Win32.HANDLE
  -> Win32.DWORD
  -> (Ptr a, Int)
  -> (Ptr b, Int)
  -> Maybe Void
  -> IO (Either Win32.ErrCode Int)
deviceIoControl h code (inPtr, inSize) (outPtr, outSize) _ = do
  with 0 $ \ lenPtr -> do
    status <- c_DeviceIoControl h code inPtr (fromIntegral inSize) outPtr
                                (fromIntegral outSize) lenPtr nullPtr
    if not status
      then do
        Left <$> Win32.getLastError
      else
        Right . fromIntegral <$> peek lenPtr

foreign import WINAPI unsafe "windows.h DeviceIoControl"
  c_DeviceIoControl
    :: Win32.HANDLE
    -> Win32.DWORD
    -> Ptr a
    -> Win32.DWORD
    -> Ptr b
    -> Win32.DWORD
    -> Ptr Win32.DWORD
    -> Ptr Void
    -> IO Win32.BOOL

readSymbolicLink :: FilePath -> IO FilePath
readSymbolicLink path = modifyIOError (`ioeSetFileName` path) $ do
  let open = Win32.createFile (toExtendedLengthPath path)
                               0 shareMode Nothing Win32.oPEN_EXISTING
                               (Win32.fILE_FLAG_BACKUP_SEMANTICS .|.
                               win32_fILE_FLAG_OPEN_REPARSE_POINT) Nothing
  bracket open Win32.closeHandle $ \ h -> do
    win32_alloca_REPARSE_DATA_BUFFER $ \ ptrAndSize@(ptr, _) -> do
      result <- deviceIoControl h win32_fSCTL_GET_REPARSE_POINT
                                (nullPtr, 0) ptrAndSize Nothing
      case result of
        Left e | e == win32_eRROR_INVALID_FUNCTION -> do
                   let msg = "Incorrect function. The file system " <>
                             "might not support symbolic links."
                   throwIO (mkIOError illegalOperationErrorType
                                      "DeviceIoControl" Nothing Nothing
                            `ioeSetErrorString` msg)
               | otherwise -> Win32.failWith "DeviceIoControl" e
        Right _ -> return ()
      rData <- win32_peek_REPARSE_DATA_BUFFER ptr
      strip <$> case rData of
        Win32_MOUNT_POINT_REPARSE_DATA_BUFFER sn _ -> pure sn
        Win32_SYMLINK_REPARSE_DATA_BUFFER sn _ _ -> pure sn
        _ -> throwIO (mkIOError InappropriateType
                                "readSymbolicLink" Nothing Nothing)
  where
    shareMode =
      win32_fILE_SHARE_DELETE .|.
      Win32.fILE_SHARE_READ   .|.
      Win32.fILE_SHARE_WRITE
    strip sn = fromMaybe sn (List.stripPrefix "\\??\\" sn)

-- | Given a list of path segments, expand @.@ and @..@.  The path segments
-- must not contain path separators.
expandDots :: [FilePath] -> [FilePath]
expandDots = reverse . go []
  where
    go ys' xs' =
      case xs' of
        [] -> ys'
        x : xs ->
          case x of
            "." -> go ys' xs
            ".." ->
              case ys' of
                _ : ys -> go ys xs
                [] -> go (x : ys') xs
            _ -> go (x : ys') xs

-- | Remove redundant trailing slashes and pick the right kind of slash.
normaliseTrailingSep :: FilePath -> FilePath
normaliseTrailingSep path = do
  let path' = reverse path
  let (sep, path'') = span isPathSeparator path'
  let addSep = if null sep then id else (pathSeparator :)
  reverse (addSep path'')

-- | A variant of 'normalise' to handle Windows paths a little better.  It
--
-- * deduplicates trailing slashes after the drive,
-- * expands parent dirs (@..@), and
-- * preserves paths with @\\\\?\\@.
normaliseW :: FilePath -> FilePath
normaliseW path@('\\' : '\\' : '?' : '\\' : _) = path
normaliseW path = normalise (joinDrive drive' subpath')
  where
    (drive, subpath) = splitDrive path
    drive' = normaliseTrailingSep drive
    subpath' = appendSep . prependSep . joinPath .
               stripPardirs . expandDots . skipSeps .
               splitDirectories $ subpath

    skipSeps = filter (not . (`elem` (pure <$> pathSeparators)))
    stripPardirs | not (isRelative path) = dropWhile (== "..")
                 | otherwise = id
    prependSep | any isPathSeparator (take 1 subpath) = (pathSeparator :)
               | otherwise = id
    appendSep | hasTrailingPathSeparator subpath = addTrailingPathSeparator
              | otherwise = id

-- | Normalise the path separators and prepend the @"\\\\?\\"@ prefix if
-- necessary or possible.  This is used for symbolic links targets because
-- they can't handle forward slashes.
normaliseSeparators :: FilePath -> FilePath
normaliseSeparators path
  | isRelative path = normaliseSep <$> path
  | otherwise = toExtendedLengthPath path
  where normaliseSep c = if isPathSeparator c then pathSeparator else c

-- | Add the @"\\\\?\\"@ prefix if necessary or possible.  The path remains
-- unchanged if the prefix is not added.  This function can sometimes be used
-- to bypass the @MAX_PATH@ length restriction in Windows API calls.
toExtendedLengthPath :: FilePath -> FilePath
toExtendedLengthPath path
  | isRelative path = path
  | otherwise =
      case normaliseW path of
        '\\' : '?'  : '?' : '\\' : _ -> path
        '\\' : '\\' : '?' : '\\' : _ -> path
        '\\' : '\\' : '.' : '\\' : _ -> path
        '\\' : subpath@('\\' : _) -> "\\\\?\\UNC" <> subpath
        normalisedPath -> "\\\\?\\" <> normalisedPath

-- | Strip the @"\\\\?\\"@ prefix if possible.
-- The prefix is kept if the meaning of the path would otherwise change.
fromExtendedLengthPath :: FilePath -> FilePath
fromExtendedLengthPath ePath =
  case ePath of
    '\\' : '\\' : '?' : '\\' : path ->
      case path of
        'U' : 'N' : 'C' : subpath@('\\' : _) -> "\\" <> subpath
        drive : ':' : subpath
          -- if the path is not "regular", then the prefix is necessary
          -- to ensure the path is interpreted literally
          | isAlpha drive && isAscii drive && isPathRegular subpath -> path
        _ -> ePath
    _ -> ePath
  where
    isPathRegular path =
      not ('/' `elem` path ||
           "." `elem` splitDirectories path ||
           ".." `elem` splitDirectories path)

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
        Left _ -> throwIO (mkIOError OtherError "" Nothing Nothing
                           `ioeSetErrorString` "path changed unexpectedly")

win32_createSymbolicLink :: String -> String -> Bool -> IO ()
win32_createSymbolicLink link _target _isDir =
#ifdef HAVE_CREATESYMBOLICLINKW
  withCWString link $ \ pLink ->
  withCWString _target $ \ pTarget -> do
    let flags = if _isDir then win32_sYMBOLIC_LINK_FLAG_DIRECTORY else 0
    status <- c_CreateSymbolicLink pLink pTarget flags
    if status == 0
      then do
        e <- Win32.getLastError
        case () of
          _ | e == win32_eRROR_INVALID_FUNCTION -> do
                let msg = "Incorrect function. The underlying file system " <>
                          "might not support symbolic links."
                throwIO (mkIOError illegalOperationErrorType
                                   "CreateSymbolicLink" Nothing (Just link)
                         `ioeSetErrorString` msg)
            | e == win32_eRROR_PRIVILEGE_NOT_HELD -> do
                let msg = "A required privilege is not held by the client. " <>
                          "Creating symbolic links usually requires " <>
                          "administrative rights."
                throwIO (mkIOError permissionErrorType "CreateSymbolicLink"
                                   Nothing (Just link)
                         `ioeSetErrorString` msg)
            | otherwise -> Win32.failWith "CreateSymbolicLink" e
      else return ()
  where

win32_eRROR_PRIVILEGE_NOT_HELD :: Win32.ErrCode
win32_eRROR_PRIVILEGE_NOT_HELD = 0x522

win32_sYMBOLIC_LINK_FLAG_DIRECTORY :: Win32.DWORD
win32_sYMBOLIC_LINK_FLAG_DIRECTORY = 0x1

foreign import WINAPI unsafe "windows.h CreateSymbolicLinkW"
  c_CreateSymbolicLink
    :: Ptr CWchar -> Ptr CWchar -> Win32.DWORD -> IO Win32.BYTE

#else
  throwIO . (`ioeSetErrorString` unsupportedErrorMsg) $
               mkIOError UnsupportedOperation "CreateSymbolicLink"
                         Nothing (Just link)
  where unsupportedErrorMsg = "Not supported on Windows XP or older"
#endif

createSymbolicLink :: Bool -> FilePath -> FilePath -> IO ()
createSymbolicLink isDir target link =
  (`ioeSetFileName` link) `modifyIOError` do
    -- normaliseSeparators ensures the target gets normalised properly
    win32_createSymbolicLink (toExtendedLengthPath link)
                             (normaliseSeparators target)
                             isDir

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
