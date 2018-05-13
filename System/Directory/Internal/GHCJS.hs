{-# LANGUAGE CPP #-}
#if defined(ghcjs_HOST_OS)
{-# LANGUAGE ForeignFunctionInterface,
             JavaScriptFFI,
             UnliftedFFITypes,
             MagicHash,
             Trustworthy,
             TupleSections
  #-}
#endif
module System.Directory.Internal.GHCJS where

#if defined(ghcjs_HOST_OS)

import Data.Bits
import Data.Maybe (listToMaybe)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX
  (POSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Foreign.C.Error
import System.IO.Error

import GHCJS.Prim

import System.Directory.Internal.Common


--------------------------------------------------------------

-- the object is treated as immutable
newtype Metadata = Metadata JSObject

getSymbolicLinkMetadata :: FilePath -> IO Metadata
getSymbolicLinkMetadata path =
  (`ioeSetFileName` path) `modifyIOError` do
    cpath <- throwErrnoIfJSNull "getSymbolicLinkMetadata" $
      js_canonicalizePath (toJSString path)
    mdata <- throwErrnoIfJSNull "getSymbolicLinkMetadata" $
      js_getFileOrSymlinkStatus cpath
    pure (Metadata mdata)

getFileMetadata :: FilePath -> IO Metadata
getFileMetadata path =
  (`ioeSetFileName` path) `modifyIOError` do
      cpath <- throwErrnoIfJSNull "getFileMetadata" $
        js_canonicalizePath (toJSString path)
      mdata <- throwErrnoIfJSNull "getFileMetadata" $
        js_getFileStatus cpath
      pure (Metadata mdata)

createSymbolicLinkInternal :: FilePath -> FilePath -> IO ()
createSymbolicLinkInternal target link =
  throwErrnoIfMinus1_ "createSymbolicLink" $
    js_createSymbolicLink (toJSString target) (toJSString link)

readSymbolicLink :: FilePath -> IO FilePath
readSymbolicLink path = do
  tgt <- throwErrnoIfJSNull "readSymbolicLink" $
    js_readSymbolicLink (toJSString path)
  pure (fromJSString tgt)

createDirectoryInternal :: FilePath -> IO ()
createDirectoryInternal path =
  throwErrnoIfMinus1_ "createDirectory" $
    js_createDirectory (toJSString path)

renamePathInternal :: FilePath -> FilePath -> IO ()
renamePathInternal opath npath =
  throwErrnoIfMinus1_ "renamePath" $
    js_renamePath (toJSString opath) (toJSString npath)

removeDirectoryInternal :: FilePath -> IO ()
removeDirectoryInternal path =
  throwErrnoIfMinus1_ "removeDirectory" $
    js_removeDirectory (toJSString path)

removeFileInternal :: FilePath -> IO ()
removeFileInternal path =
  throwErrnoIfMinus1_ "removeFile" $
    js_removeFile (toJSString path)

canonicalizePathInternal :: FilePath -> IO FilePath
canonicalizePathInternal path = do
  cpath <- throwErrnoIfJSNull "canonicalizePath" $
    js_canonicalizePath (toJSString path)
  pure (fromJSString cpath)

findExecutablesInternal :: FilePath -> IO [FilePath]
findExecutablesInternal binary = do
  arr <- throwErrnoIfJSNull "findExecutables" $
    js_findExecutables (toJSString binary)
  fmap (map fromJSString) (fromJSArray arr)

getDirectoryContentsInternal :: FilePath -> IO [FilePath]
getDirectoryContentsInternal path = do
  arr <- throwErrnoIfJSNull "getDirectoryContents" $
    js_getDirectoryContents (toJSString path)
  fmap (map fromJSString) (fromJSArray arr)

setCurrentDirectoryInternal :: FilePath -> IO ()
setCurrentDirectoryInternal path =
  throwErrnoIfMinus1_ "setCurrentDirectory" $
    js_setCurrentDirectory (toJSString path)

setFileTimesInternal :: FilePath -> (Maybe UTCTime, Maybe UTCTime) -> IO ()
setFileTimesInternal path (atime, mtime) =
  throwErrnoIfMinus1_ "setFileTimes" $
    js_setFileTimes (toJSString path) atime' set_atime mtime' set_mtime
  where
    f = maybe (0, False) ((,True) . realToFrac . utcTimeToPOSIXSeconds)
    (atime', set_atime) = f atime
    (mtime', set_mtime) = f mtime

fileTypeFromMetadata :: Metadata -> FileType
fileTypeFromMetadata info
  | isLink    = if isDir then DirectoryLink else SymbolicLink
  | isDir     = Directory
  | otherwise = File
  where
    isLink = js_getFileStatusIsSymbolicLink info
    isDir  = js_getFileStatusIsDirectory info

fileSizeFromMetadata :: Metadata -> Integer
fileSizeFromMetadata = round . js_getFileStatusFileSize

accessTimeFromMetadata :: Metadata -> UTCTime
accessTimeFromMetadata =
  posixSecondsToUTCTime . realToFrac . js_getFileStatusAccessTime

modificationTimeFromMetadata :: Metadata -> UTCTime
modificationTimeFromMetadata =
  posixSecondsToUTCTime . realToFrac . js_getFileStatusModificationTime

type Mode = Int

ownerReadMode, groupReadMode, otherReadMode :: Mode
ownerReadMode = 0o400
groupReadMode = 0o040
otherReadMode = 0o004

ownerWriteMode, groupWriteMode, otherWriteMode :: Mode
ownerWriteMode = 0o200
groupWriteMode = 0o020
otherWriteMode = 0o002

ownerExecuteMode, groupExecuteMode, otherExecuteMode :: Mode
ownerExecuteMode = 0o100
groupExecuteMode = 0o010
otherExecuteMode = 0o001

modeFromMetadata :: Metadata -> Mode
modeFromMetadata = js_getFileStatusFileMode

allWriteMode :: Mode
allWriteMode =
  ownerWriteMode .|.
  groupWriteMode .|.
  otherWriteMode

hasWriteMode :: Mode -> Bool
hasWriteMode m = m .&. allWriteMode /= 0

setWriteMode :: Bool -> Mode -> Mode
setWriteMode False m = m .&. complement allWriteMode
setWriteMode True  m = m .|. allWriteMode

setFileMode :: FilePath -> Mode -> IO ()
setFileMode path mode =
  throwErrnoIfMinus1_ "setFileMode" $
    js_setFileMode (toJSString path) mode

setFilePermissions :: FilePath -> Mode -> IO ()
setFilePermissions = setFileMode

fileAccess :: FilePath -> Bool -> Bool -> Bool -> IO Bool
fileAccess path r_ok w_ok x_ok = fileAccess' (toJSString path) r_ok w_ok x_ok

fileAccess' :: JSString -> Bool -> Bool -> Bool -> IO Bool
fileAccess' path r_ok w_ok x_ok = js_getFileAccess path r_ok w_ok x_ok

getAccessPermissions :: FilePath -> IO Permissions
getAccessPermissions path = do
  m <- getFileMetadata path
  let isDir = fileTypeIsDirectory (fileTypeFromMetadata m)
      path' = toJSString path
  r <- fileAccess' path' True  False False
  w <- fileAccess' path' False True  False
  x <- fileAccess' path' False False True
  return Permissions
         { readable   = r
         , writable   = w
         , executable = x && not isDir
         , searchable = x && isDir
         }

setAccessPermissions :: FilePath -> Permissions -> IO ()
setAccessPermissions path (Permissions r w e s) = do
  m <- getFileMetadata path
  setFileMode path (modifyBit (e || s) ownerExecuteMode .
                    modifyBit w ownerWriteMode .
                    modifyBit r ownerReadMode .
                    modeFromMetadata $ m)
  where
    modifyBit :: Bool -> Mode -> Mode -> Mode
    modifyBit False b m = m .&. complement b
    modifyBit True  b m = m .|. b


foreign import javascript interruptible
  "h$directory_getPermissions($1,$c);"
  js_getPermissions :: JSString -> IO Int

foreign import javascript interruptible
  "h$directory_setPermissions($1,$2,$c);"
  js_setPermissions :: JSString -> Int -> IO Int

foreign import javascript interruptible
  "h$directory_copyPermissions($1,$2,$c);"
  js_copyPermissions :: JSString -> JSString -> IO Int

foreign import javascript interruptible
  "h$directory_createDirectory($1,$c);"
  js_createDirectory :: JSString -> IO Int

foreign import javascript interruptible
  "h$directory_createSymbolicLink($1,$2,$c);"
  js_createSymbolicLink :: JSString -> JSString -> IO Int

foreign import javascript interruptible
  "h$directory_removeDirectory($1,$c);"
  js_removeDirectory :: JSString -> IO Int

foreign import javascript interruptible
  "h$directory_removeFile($1,$c);"
  js_removeFile :: JSString -> IO Int

foreign import javascript interruptible
  "h$directory_renamePath($1,$2,$c);"
  js_renamePath :: JSString -> JSString -> IO Int

foreign import javascript unsafe
  "h$directory_canonicalizePath($1)"
  js_canonicalizePath :: JSString -> IO JSString

foreign import javascript interruptible
  "h$directory_findExecutables($1,$c);"
  js_findExecutables :: JSString -> IO JSArray

foreign import javascript interruptible
  "h$directory_getDirectoryContents($1,$c);"
  js_getDirectoryContents :: JSString -> IO JSArray

foreign import javascript interruptible
  "h$directory_copyFileWithMetadata($1,$2,$c);"
  js_copyFileWithMetadata :: JSString -> JSString -> IO ()

foreign import javascript unsafe
  "h$directory_setCurrentDirectory($1)"
  js_setCurrentDirectory :: JSString -> IO Int

foreign import javascript unsafe
  "h$directory_getHomeDirectory()"
  js_getHomeDirectory :: IO JSString

foreign import javascript unsafe
  "h$directory_getAppUserDataDirectory($1)"
  js_getAppUserDataDirectory :: JSString -> IO JSString

foreign import javascript unsafe
  "h$directory_getUserDocumentsDirectory()"
  js_getUserDocumentsDirectory :: IO JSString

foreign import javascript unsafe
  "h$directory_getTemporaryDirectory()"
  js_getTemporaryDirectory :: IO JSString

foreign import javascript interruptible
  "h$directory_getFileStatus($1,$c);"
  js_getFileStatus :: JSString -> IO JSVal

foreign import javascript interruptible
  "h$directory_getFileOrSymlinkStatus($1,$c);"
  js_getFileOrSymlinkStatus :: JSString -> IO JSVal

foreign import javascript unsafe
  "h$directory_getFileStatusModificationTime($1)"
  js_getFileStatusModificationTime :: Metadata -> Double

foreign import javascript unsafe
  "h$directory_getFileStatusAccessTime($1)"
  js_getFileStatusAccessTime :: Metadata -> Double

foreign import javascript unsafe
  "h$directory_getFileStatusIsDirectory($1)"
  js_getFileStatusIsDirectory :: Metadata -> Bool

foreign import javascript unsafe
  "h$directory_getFileStatusIsSymbolicLink($1)"
  js_getFileStatusIsSymbolicLink :: Metadata -> Bool

foreign import javascript unsafe
  "h$directory_getFileStatusFileMode($1)"
  js_getFileStatusFileMode :: Metadata -> Int

foreign import javascript unsafe
  "h$directory_getFileStatusFileSize($1)"
  js_getFileStatusFileSize :: Metadata -> Double

foreign import javascript interruptible
  "h$directory_getFileAccess($1,$2,$3,$4,$c);"
  js_getFileAccess :: JSString -> Bool -> Bool -> Bool -> IO Bool

foreign import javascript interruptible
  "h$directory_setFileMode($1,$2,$c);"
  js_setFileMode :: JSString -> Mode -> IO Int

foreign import javascript interruptible
  "h$directory_readSymbolicLink($1,$c);"
  js_readSymbolicLink :: JSString -> IO JSString

foreign import javascript interruptible
  "h$directory_setFileTimes($1,$2,$3,$4,$5,$c);"
  js_setFileTimes :: JSString -> Double -> Bool -> Double -> Bool -> IO Int
#endif
