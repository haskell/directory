{-# LANGUAGE CPP #-}
module System.Directory.Internal.Windows where
#include <HsDirectoryConfig.h>
#if defined(mingw32_HOST_OS)
##if defined(i386_HOST_ARCH)
## define WINAPI stdcall
##elif defined(x86_64_HOST_ARCH)
## define WINAPI ccall
##else
## error unknown architecture
##endif
#include <shlobj.h>
#include <windows.h>
#include <HsBaseConfig.h>
#include <System/Directory/Internal/utility.h>
#include <System/Directory/Internal/windows_ext.h>
import Prelude ()
import System.Directory.Internal.Prelude
import System.Directory.Internal.Common
import System.Directory.Internal.Config (exeExtension)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
#ifdef __IO_MANAGER_WINIO__
import GHC.IO.SubSystem (IoSubSystem(IoPOSIX, IoNative), ioSubSystem)
#endif
import System.OsPath
  ( (</>)
  , isPathSeparator
  , isRelative
  , pack
  , pathSeparator
  , splitDirectories
  , takeExtension
  , toChar
  , unpack
  )
import System.OsPath.Types (WindowsPath, WindowsString)
import System.OsString.Internal.Types (OsString(OsString, getOsString))
import qualified Data.List as List
import qualified System.Win32.WindowsString.File as Win32
import qualified System.Win32.WindowsString.Info as Win32
import qualified System.Win32.WindowsString.Shell as Win32
import qualified System.Win32.WindowsString.Time as Win32
import qualified System.Win32.WindowsString.Types as Win32

createDirectoryInternal :: OsPath -> IO ()
createDirectoryInternal path =
  (`ioeSetOsPath` path) `modifyIOError` do
    path' <- furnishPath path
    Win32.createDirectory path' Nothing

removePathInternal :: Bool -> OsPath -> IO ()
removePathInternal isDir path =
  (`ioeSetOsPath` path) `modifyIOError` do
    furnishPath path
      >>= if isDir then Win32.removeDirectory else Win32.deleteFile

renamePathInternal :: OsPath -> OsPath -> IO ()
renamePathInternal opath npath =
  (`ioeSetOsPath` opath) `modifyIOError` do
    opath' <- furnishPath opath
    npath' <- furnishPath npath
    Win32.moveFileEx opath' (Just npath') Win32.mOVEFILE_REPLACE_EXISTING

-- On Windows, the removability of a file may be affected by the attributes of
-- the file itself.
filesAlwaysRemovable :: Bool
filesAlwaysRemovable = False

copyFileWithMetadataInternal :: (Metadata -> OsPath -> IO ())
                             -> (Metadata -> OsPath -> IO ())
                             -> OsPath
                             -> OsPath
                             -> IO ()
copyFileWithMetadataInternal _ _ src dst =
  (`ioeSetOsPath` src) `modifyIOError` do
    src' <- furnishPath src
    dst' <- furnishPath dst
    Win32.copyFile src' dst' False

win32_cSIDL_COMMON_APPDATA :: Win32.CSIDL
win32_cSIDL_COMMON_APPDATA = (#const CSIDL_COMMON_APPDATA)

win32_eRROR_ENVVAR_NOT_FOUND :: Win32.ErrCode
win32_eRROR_ENVVAR_NOT_FOUND = (#const ERROR_ENVVAR_NOT_FOUND)

win32_eRROR_INVALID_FUNCTION :: Win32.ErrCode
win32_eRROR_INVALID_FUNCTION = (#const ERROR_INVALID_FUNCTION)

win32_eRROR_INVALID_PARAMETER :: Win32.ErrCode
win32_eRROR_INVALID_PARAMETER = (#const ERROR_INVALID_PARAMETER)

win32_eRROR_PRIVILEGE_NOT_HELD :: Win32.ErrCode
win32_eRROR_PRIVILEGE_NOT_HELD = (#const ERROR_PRIVILEGE_NOT_HELD)

win32_sYMBOLIC_LINK_FLAG_DIRECTORY :: Win32.DWORD
win32_sYMBOLIC_LINK_FLAG_DIRECTORY = 0x1

win32_sYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE :: Win32.DWORD
win32_sYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE = 0x2

maxShareMode :: Win32.ShareMode
maxShareMode =
  Win32.fILE_SHARE_DELETE .|.
  Win32.fILE_SHARE_READ   .|.
  Win32.fILE_SHARE_WRITE

openFileForRead :: OsPath -> IO Handle
openFileForRead (OsString path) =
  bracketOnError
    (Win32.createFile
      path
      Win32.gENERIC_READ
      maxShareMode
      Nothing
      Win32.oPEN_ALWAYS
      (Win32.fILE_ATTRIBUTE_NORMAL .|. possiblyOverlapped)
      Nothing)
    Win32.closeHandle
    Win32.hANDLEToHandle

possiblyOverlapped :: Win32.FileAttributeOrFlag
#ifdef __IO_MANAGER_WINIO__
possiblyOverlapped | ioSubSystem == IoNative = Win32.fILE_FLAG_OVERLAPPED
                   | otherwise               = 0
#else
possiblyOverlapped = 0
#endif

win32_getFinalPathNameByHandle :: Win32.HANDLE -> Win32.DWORD -> IO WindowsPath
#ifdef HAVE_GETFINALPATHNAMEBYHANDLEW
win32_getFinalPathNameByHandle h flags = do
  result <- peekTStringWith (#const MAX_PATH) $ \ ptr len -> do
    c_GetFinalPathNameByHandle h ptr len flags
  case result of
    Left errCode -> Win32.failWith "GetFinalPathNameByHandle" errCode
    Right path -> pure path

foreign import WINAPI unsafe "windows.h GetFinalPathNameByHandleW"
  c_GetFinalPathNameByHandle
    :: Win32.HANDLE
    -> Ptr CWchar
    -> Win32.DWORD
    -> Win32.DWORD
    -> IO Win32.DWORD

#else
win32_getFinalPathNameByHandle _ _ = throwIO $
  mkIOError
    UnsupportedOperation
    "platform does not support GetFinalPathNameByHandle"
    Nothing
    Nothing
#endif

getFinalPathName :: OsPath -> IO OsPath
getFinalPathName =
  (fromExtendedLengthPath <$>) .
  rawGetFinalPathName .
  toExtendedLengthPath
  where
#ifdef HAVE_GETFINALPATHNAMEBYHANDLEW
    rawGetFinalPathName path = do
      let open = Win32.createFile path 0 maxShareMode Nothing
                 Win32.oPEN_EXISTING Win32.fILE_FLAG_BACKUP_SEMANTICS Nothing
      bracket open Win32.closeHandle $ \ h -> do
        win32_getFinalPathNameByHandle h 0
#else
    rawGetFinalPathName = Win32.getLongPathName <=< Win32.getShortPathName
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
  = Win32_MOUNT_POINT_REPARSE_DATA_BUFFER WindowsString WindowsString
    -- ^ substituteName printName
  | Win32_SYMLINK_REPARSE_DATA_BUFFER WindowsString WindowsString Bool
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
    peekName :: Ptr CWchar -> CUShort -> CUShort -> IO WindowsString
    peekName buf offset size =
      Win32.peekTStringLen ( buf `plusPtr` fromIntegral offset
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
    ok <- c_DeviceIoControl h code inPtr (fromIntegral inSize) outPtr
                            (fromIntegral outSize) lenPtr nullPtr
    if ok
      then Right . fromIntegral <$> peek lenPtr
      else Left <$> Win32.getLastError

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

readSymbolicLink :: OsPath -> IO OsPath
readSymbolicLink path =
  (`ioeSetOsPath` path) `modifyIOError` do
    path' <- furnishPath path
    let open = Win32.createFile path' 0 maxShareMode Nothing Win32.oPEN_EXISTING
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
          Right _ -> pure ()
        rData <- win32_peek_REPARSE_DATA_BUFFER ptr
        strip . OsString <$> case rData of
          Win32_MOUNT_POINT_REPARSE_DATA_BUFFER sn _ -> pure sn
          Win32_SYMLINK_REPARSE_DATA_BUFFER sn _ _ -> pure sn
          _ -> throwIO (mkIOError InappropriateType
                                  "readSymbolicLink" Nothing Nothing)
  where
    strip sn =
      fromMaybe sn
        (pack <$> List.stripPrefix (unpack (os "\\??\\")) (unpack sn))

-- | On Windows, equivalent to 'simplifyWindows'.
simplify :: OsPath -> OsPath
simplify = simplifyWindows

-- | Normalise the path separators and prepend the @"\\\\?\\"@ prefix if
-- necessary or possible.  This is used for symbolic links targets because
-- they can't handle forward slashes.
normaliseSeparators :: OsPath -> WindowsPath
normaliseSeparators path
  | isRelative path = getOsString (pack (normaliseSep <$> unpack path))
  | otherwise = toExtendedLengthPath path
  where normaliseSep c = if isPathSeparator c then pathSeparator else c

-- | 'simplify' the path and prepend the @"\\\\?\\"@ if possible.  This
-- function can sometimes be used to bypass the @MAX_PATH@ length restriction
-- in Windows API calls.
toExtendedLengthPath :: OsPath -> WindowsPath
toExtendedLengthPath path =
  getOsString $
  if isRelative path
  then simplifiedPath
  else
    case toChar <$> simplifiedPath' of
      '\\' : '?'  : '?' : '\\' : _ -> simplifiedPath
      '\\' : '\\' : '?' : '\\' : _ -> simplifiedPath
      '\\' : '\\' : '.' : '\\' : _ -> simplifiedPath
      '\\' : '\\' : _ ->
        os "\\\\?\\UNC" <> pack (drop 1 simplifiedPath')
      _ -> os "\\\\?\\" <> simplifiedPath
  where simplifiedPath = simplify path
        simplifiedPath' = unpack simplifiedPath

-- | Make a path absolute and convert to an extended length path, if possible.
--
-- Empty paths are left unchanged.
--
-- This function never fails.  If it doesn't understand the path, it just
-- returns the path unchanged.
furnishPath :: OsPath -> IO WindowsPath
furnishPath path =
  (toExtendedLengthPath <$> rawPrependCurrentDirectory path)
    `catchIOError` \ _ ->
      pure (getOsString path)

-- | Strip the @"\\\\?\\"@ prefix if possible.
-- The prefix is kept if the meaning of the path would otherwise change.
fromExtendedLengthPath :: WindowsPath -> OsPath
fromExtendedLengthPath ePath' =
  case unpack ePath of
    c1 : c2 : c3 : c4 : path
      | (toChar <$> [c1, c2, c3, c4]) == "\\\\?\\" ->
      case path of
        c5 : c6 : c7 : subpath@(c8 : _)
          | (toChar <$> [c5, c6, c7, c8]) == "UNC\\" ->
            os "\\" <> pack subpath
        drive : col : subpath
          -- if the path is not "regular", then the prefix is necessary
          -- to ensure the path is interpreted literally
          | toChar col == ':', isDriveChar drive, isPathRegular subpath ->
            pack path
        _ -> ePath
    _ -> ePath
  where
    ePath = OsString ePath'
    isDriveChar drive = isAlpha (toChar drive) && isAscii (toChar drive)
    isPathRegular path =
      not ('/' `elem` (toChar <$> path) ||
           os "." `elem` splitDirectories (pack path) ||
           os ".." `elem` splitDirectories (pack path))

saturatingDouble :: Win32.DWORD -> Win32.DWORD
saturatingDouble s | s > maxBound `div` 2 = maxBound
                   | otherwise            = s * 2

-- Handles Windows APIs that write strings through a user-provided buffer and
-- can propose a new length when it isn't big enough. This is similar to
-- Win32.try, but also returns the precise error code.
peekTStringWith :: Win32.DWORD
                -> (Win32.LPTSTR -> Win32.DWORD -> IO Win32.DWORD)
                -- ^ Must accept a buffer and its size in TCHARs. If the
                --   buffer is large enough for the function, it must write a
                --   string to it, which need not be null-terminated, and
                --   return the length of the string, not including the null
                --   terminator if present. If the buffer is too small, it
                --   must return a proposed buffer size in TCHARs, although it
                --   need not guarantee success with the proposed size if,
                --   say, the underlying data changes in the interim. If it
                --   fails for any other reason, it must return zero and
                --   communicate the error code through GetLastError.
                -> IO (Either Win32.ErrCode WindowsPath)
peekTStringWith bufferSize cFunc = do
  outcome <- do
    allocaArray (fromIntegral bufferSize) $ \ ptr -> do
      size <- cFunc ptr bufferSize
      case size of
        0 -> Right . Left <$> Win32.getLastError
        _ | size <= bufferSize ->
              Right . Right <$> Win32.peekTStringLen (ptr, fromIntegral size)
          | otherwise ->
              -- At least double the size to ensure fast termination.
              pure (Left (max size (saturatingDouble bufferSize)))
  case outcome of
    Left proposedSize -> peekTStringWith proposedSize cFunc
    Right result      -> pure result

realPath :: OsPath -> IO OsPath
realPath = getFinalPathName

canonicalizePathSimplify :: OsPath -> IO OsPath
canonicalizePathSimplify path =
  getFullPathName path
    `catchIOError` \ _ ->
      pure path

searchPathEnvForExes :: OsString -> IO (Maybe OsPath)
searchPathEnvForExes (OsString binary) =
  (OsString <$>) <$>
    Win32.searchPath Nothing binary (Just (getOsString exeExtension))

findExecutablesLazyInternal :: ([OsPath] -> OsString -> ListT IO OsPath)
                            -> OsString
                            -> ListT IO OsPath
findExecutablesLazyInternal _ = maybeToListT . searchPathEnvForExes

exeExtensionInternal :: OsString
exeExtensionInternal = exeExtension

getDirectoryContentsInternal :: OsPath -> IO [OsPath]
getDirectoryContentsInternal path = do
  query <- furnishPath (path </> os "*")
  bracket
    (Win32.findFirstFile query)
    (\ (h, _) -> Win32.findClose h)
    (\ (h, fdat) -> loop h fdat [])
  where
    -- we needn't worry about empty directories: a directory always
    -- has at least "." and ".." entries
    loop :: Win32.HANDLE -> Win32.FindData -> [OsPath] -> IO [OsPath]
    loop h fdat acc = do
      filename <- Win32.getFindDataFileName fdat
      more <- Win32.findNextFile h fdat
      if more
        then loop h fdat (OsString filename : acc)
        else pure (OsString filename : acc)
             -- no need to reverse, ordering is undefined

getCurrentDirectoryInternal :: IO OsPath
getCurrentDirectoryInternal = OsString <$> Win32.getCurrentDirectory

getFullPathName :: OsPath -> IO OsPath
getFullPathName path =
  fromExtendedLengthPath <$> Win32.getFullPathName (toExtendedLengthPath path)

-- | Similar to 'prependCurrentDirectory' but fails for empty paths.
rawPrependCurrentDirectory :: OsPath -> IO OsPath
rawPrependCurrentDirectory path
  | isRelative path =
    ((`ioeAddLocation` "prependCurrentDirectory") .
     (`ioeSetOsPath` path)) `modifyIOError` do
      getFullPathName path
  | otherwise = pure path

-- | Convert a path into an absolute path.  If the given path is relative, the
-- current directory is prepended and the path may or may not be simplified.
-- If the path is already absolute, the path is returned unchanged.  The
-- function preserves the presence or absence of the trailing path separator.
--
-- If the path is already absolute, the operation never fails.  Otherwise, the
-- operation may throw exceptions.
--
-- Empty paths are treated as the current directory.
prependCurrentDirectory :: OsPath -> IO OsPath
prependCurrentDirectory = rawPrependCurrentDirectory . emptyToCurDir

-- SetCurrentDirectory does not support long paths even with the \\?\ prefix
-- https://ghc.haskell.org/trac/ghc/ticket/13373#comment:6
setCurrentDirectoryInternal :: OsPath -> IO ()
setCurrentDirectoryInternal = Win32.setCurrentDirectory . getOsString

createSymbolicLinkUnpriv :: WindowsPath -> WindowsPath -> Bool -> IO ()
createSymbolicLinkUnpriv link _target _isDir =
#ifdef HAVE_CREATESYMBOLICLINKW
  Win32.withTString link $ \ pLink ->
  Win32.withTString _target $ \ pTarget -> do
    let flags = if _isDir then win32_sYMBOLIC_LINK_FLAG_DIRECTORY else 0
    call pLink pTarget flags win32_sYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE
  where
    call pLink pTarget flags unpriv = do
      status <- c_CreateSymbolicLink pLink pTarget (flags .|. unpriv)
      when (status == 0) $ do
        e <- Win32.getLastError
        case () of
          _ | e == win32_eRROR_INVALID_FUNCTION -> do
                let msg = "Incorrect function. The underlying file system " <>
                          "might not support symbolic links."
                throwIO (mkIOError illegalOperationErrorType
                                   "CreateSymbolicLink" Nothing Nothing
                         `ioeSetOsPath` OsString link
                         `ioeSetErrorString` msg)
            | e == win32_eRROR_PRIVILEGE_NOT_HELD -> do
                let msg = "A required privilege is not held by the client. " <>
                          "Creating symbolic links usually requires " <>
                          "administrative rights."
                throwIO (mkIOError permissionErrorType "CreateSymbolicLink"
                                   Nothing Nothing
                         `ioeSetOsPath` OsString link
                         `ioeSetErrorString` msg)
            | e == win32_eRROR_INVALID_PARAMETER &&
              unpriv /= 0 ->
                -- for compatibility with older versions of Windows,
                -- try it again without the flag
                call pLink pTarget flags 0
            | otherwise -> Win32.failWith "CreateSymbolicLink" e

foreign import WINAPI unsafe "windows.h CreateSymbolicLinkW"
  c_CreateSymbolicLink
    :: Ptr CWchar -> Ptr CWchar -> Win32.DWORD -> IO Win32.BYTE

#else
  throwIO . (`ioeSetErrorString` unsupportedErrorMsg)
          . (`ioeSetOsPath` OsString link) $
               mkIOError UnsupportedOperation "CreateSymbolicLink"
                         Nothing Nothing
  where unsupportedErrorMsg = "Not supported on Windows XP or older"
#endif

linkToDirectoryIsDirectory :: Bool
linkToDirectoryIsDirectory = True

createSymbolicLink :: Bool -> OsPath -> OsPath -> IO ()
createSymbolicLink isDir target link =
  (`ioeSetOsPath` link) `modifyIOError` do
    -- normaliseSeparators ensures the target gets normalised properly
    link' <- furnishPath link
    createSymbolicLinkUnpriv
      link'
      (normaliseSeparators target)
      isDir

type Metadata = Win32.BY_HANDLE_FILE_INFORMATION

getSymbolicLinkMetadata :: OsPath -> IO Metadata
getSymbolicLinkMetadata path =
  (`ioeSetOsPath` path) `modifyIOError` do
    path' <- furnishPath path
    let open = Win32.createFile path' 0 maxShareMode Nothing Win32.oPEN_EXISTING
                                (Win32.fILE_FLAG_BACKUP_SEMANTICS .|.
                                 win32_fILE_FLAG_OPEN_REPARSE_POINT) Nothing
    bracket open Win32.closeHandle $ \ h -> do
      Win32.getFileInformationByHandle h

getFileMetadata :: OsPath -> IO Metadata
getFileMetadata path =
  (`ioeSetOsPath` path) `modifyIOError` do
    path' <- furnishPath path
    let open = Win32.createFile path' 0 maxShareMode Nothing Win32.oPEN_EXISTING
                                Win32.fILE_FLAG_BACKUP_SEMANTICS Nothing
    bracket open Win32.closeHandle $ \ h -> do
      Win32.getFileInformationByHandle h

fileTypeFromMetadata :: Metadata -> FileType
fileTypeFromMetadata info
  | isLink    = if isDir then DirectoryLink else SymbolicLink
  | isDir     = Directory
  | otherwise = File
  where
    isLink = attrs .&. Win32.fILE_ATTRIBUTE_REPARSE_POINT /= 0
    isDir  = attrs .&. Win32.fILE_ATTRIBUTE_DIRECTORY /= 0
    attrs  = Win32.bhfiFileAttributes info

fileSizeFromMetadata :: Metadata -> Integer
fileSizeFromMetadata = fromIntegral . Win32.bhfiSize

accessTimeFromMetadata :: Metadata -> UTCTime
accessTimeFromMetadata =
  posixSecondsToUTCTime . windowsToPosixTime . Win32.bhfiLastAccessTime

modificationTimeFromMetadata :: Metadata -> UTCTime
modificationTimeFromMetadata =
  posixSecondsToUTCTime . windowsToPosixTime . Win32.bhfiLastWriteTime

-- | Difference between the Windows and POSIX epochs in units of 100ns.
windowsPosixEpochDifference :: Num a => a
windowsPosixEpochDifference = 116444736000000000

-- | Convert from Windows time to POSIX time.
windowsToPosixTime :: Win32.FILETIME -> POSIXTime
windowsToPosixTime (Win32.FILETIME t) =
  (fromIntegral t - windowsPosixEpochDifference) / 10000000

-- | Convert from POSIX time to Windows time.  This is lossy as Windows time
--   has a resolution of only 100ns.
posixToWindowsTime :: POSIXTime -> Win32.FILETIME
posixToWindowsTime t = Win32.FILETIME $
  truncate (t * 10000000 + windowsPosixEpochDifference)

setTimes :: OsPath -> (Maybe POSIXTime, Maybe POSIXTime) -> IO ()
setTimes path' (atime', mtime') =
  bracket (openFileHandle path' Win32.gENERIC_WRITE)
          Win32.closeHandle $ \ handle ->
  Win32.setFileTime handle Nothing (posixToWindowsTime <$> atime') (posixToWindowsTime <$> mtime')

-- | Open the handle of an existing file or directory.
openFileHandle :: OsString -> Win32.AccessMode -> IO Win32.HANDLE
openFileHandle path mode =
  (`ioeSetOsPath` path) `modifyIOError` do
    path' <- furnishPath path
    Win32.createFile path' mode maxShareMode Nothing
                     Win32.oPEN_EXISTING flags Nothing
  where flags =  Win32.fILE_ATTRIBUTE_NORMAL
             .|. Win32.fILE_FLAG_BACKUP_SEMANTICS -- required for directories

type Mode = Win32.FileAttributeOrFlag

modeFromMetadata :: Metadata -> Mode
modeFromMetadata = Win32.bhfiFileAttributes

hasWriteMode :: Mode -> Bool
hasWriteMode m = m .&. Win32.fILE_ATTRIBUTE_READONLY == 0

setWriteMode :: Bool -> Mode -> Mode
setWriteMode False m = m .|. Win32.fILE_ATTRIBUTE_READONLY
setWriteMode True  m = m .&. complement Win32.fILE_ATTRIBUTE_READONLY

setFileMode :: OsPath -> Mode -> IO ()
setFileMode path mode =
  (`ioeSetOsPath` path) `modifyIOError` do
    path' <- furnishPath path
    Win32.setFileAttributes path' mode

-- | A restricted form of 'setFileMode' that only sets the permission bits.
-- For Windows, this means only the "read-only" attribute is affected.
setFilePermissions :: OsPath -> Mode -> IO ()
setFilePermissions path m = do
  m' <- modeFromMetadata <$> getFileMetadata path
  setFileMode path ((m' .&. complement Win32.fILE_ATTRIBUTE_READONLY) .|.
                    (m  .&. Win32.fILE_ATTRIBUTE_READONLY))

getAccessPermissions :: OsPath -> IO Permissions
getAccessPermissions path = do
  m <- getFileMetadata path
  let isDir = fileTypeIsDirectory (fileTypeFromMetadata m)
  let w = hasWriteMode (modeFromMetadata m)
  let x = (toLower . toChar <$> unpack (takeExtension path))
          `elem` [".bat", ".cmd", ".com", ".exe"]
  pure Permissions
       { readable   = True
       , writable   = w
       , executable = x && not isDir
       , searchable = isDir
       }

setAccessPermissions :: OsPath -> Permissions -> IO ()
setAccessPermissions path Permissions{writable = w} = do
  setFilePermissions path (setWriteMode w 0)

lookupEnvOs :: OsString -> IO (Maybe OsString)
lookupEnvOs (OsString name) = do
  result <-
    Win32.withTString name $ \ pName ->
    peekTStringWith 256 $ \ pBuffer size ->
    c_GetEnvironmentVariable pName pBuffer size
  case result of
    Left errCode | errCode == win32_eRROR_ENVVAR_NOT_FOUND -> pure Nothing
                 | otherwise -> Win32.failWith "GetEnvironmentVariable" errCode
    Right value -> pure (Just (OsString value))

foreign import WINAPI unsafe "windows.h GetEnvironmentVariableW"
  c_GetEnvironmentVariable
    :: Win32.LPWSTR
    -> Win32.LPWSTR
    -> Win32.DWORD
    -> IO Win32.DWORD

getFolderPath :: Win32.CSIDL -> IO OsPath
getFolderPath what = OsString <$> Win32.sHGetFolderPath nullPtr what nullPtr 0

getHomeDirectoryInternal :: IO OsPath
getHomeDirectoryInternal =
  getFolderPath Win32.cSIDL_PROFILE `catchIOError` \ _ ->
    getFolderPath Win32.cSIDL_WINDOWS

getXdgDirectoryFallback :: IO OsPath -> XdgDirectory -> IO OsPath
getXdgDirectoryFallback _ xdgDir = do
  case xdgDir of
    XdgData   -> getFolderPath Win32.cSIDL_APPDATA
    XdgConfig -> getFolderPath Win32.cSIDL_APPDATA
    XdgCache  -> getFolderPath Win32.cSIDL_LOCAL_APPDATA
    XdgState  -> getFolderPath Win32.cSIDL_LOCAL_APPDATA

getXdgDirectoryListFallback :: XdgDirectoryList -> IO [OsPath]
getXdgDirectoryListFallback _ =
  pure <$> getFolderPath win32_cSIDL_COMMON_APPDATA

getAppUserDataDirectoryInternal :: OsPath -> IO OsPath
getAppUserDataDirectoryInternal appName =
  (\ appData -> appData <> (os "\\" <> appName))
  <$> getXdgDirectoryFallback getHomeDirectoryInternal XdgData

getUserDocumentsDirectoryInternal :: IO OsPath
getUserDocumentsDirectoryInternal = getFolderPath Win32.cSIDL_PERSONAL

getTemporaryDirectoryInternal :: IO OsPath
getTemporaryDirectoryInternal = OsString <$> Win32.getTemporaryDirectory

#endif
