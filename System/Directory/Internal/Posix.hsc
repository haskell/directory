module System.Directory.Internal.Posix where
#include <HsDirectoryConfig.h>
#if !defined(mingw32_HOST_OS)
#ifdef HAVE_LIMITS_H
# include <limits.h>
#endif
import Prelude ()
import System.Directory.Internal.Prelude
#ifdef HAVE_UTIMENSAT
import System.Directory.Internal.C_utimensat
#endif
import System.Directory.Internal.Common
import System.Directory.Internal.Config (exeExtension)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime)
import System.OsPath ((</>), encodeFS, isRelative, splitSearchPath)
import System.OsString.Internal.Types (OsString(OsString, getOsString))
import qualified Data.Time.Clock.POSIX as POSIXTime
import qualified System.Posix.Directory.PosixPath as Posix
import qualified System.Posix.Env.PosixString as Posix
import qualified System.Posix.Files.PosixString as Posix
import qualified System.Posix.IO.PosixString as Posix
import qualified System.Posix.PosixPath.FilePath as Posix
import qualified System.Posix.Types as Posix
import qualified System.Posix.User as Posix

createDirectoryInternal :: OsPath -> IO ()
createDirectoryInternal (OsString path) = Posix.createDirectory path 0o777

removePathInternal :: Bool -> OsPath -> IO ()
removePathInternal True  = Posix.removeDirectory . getOsString
removePathInternal False = Posix.removeLink . getOsString

renamePathInternal :: OsPath -> OsPath -> IO ()
renamePathInternal (OsString p1) (OsString p2) = Posix.rename p1 p2

-- On POSIX, the removability of a file is only affected by the attributes of
-- the containing directory.
filesAlwaysRemovable :: Bool
filesAlwaysRemovable = True

-- | On POSIX, equivalent to 'simplifyPosix'.
simplify :: OsPath -> OsPath
simplify = simplifyPosix

-- we use the 'free' from the standard library here since it's not entirely
-- clear whether Haskell's 'free' corresponds to the same one
foreign import ccall unsafe "free" c_free :: Ptr a -> IO ()

c_PATH_MAX :: Maybe Int
#ifdef PATH_MAX
c_PATH_MAX | c_PATH_MAX' > toInteger maxValue = Nothing
           | otherwise                        = Just (fromInteger c_PATH_MAX')
  where c_PATH_MAX' = (#const PATH_MAX)
        maxValue = maxBound `asTypeInMaybe` c_PATH_MAX
        asTypeInMaybe :: a -> Maybe a -> a
        asTypeInMaybe = const
#else
c_PATH_MAX = Nothing
#endif

#if !defined(HAVE_REALPATH)

c_realpath :: CString -> CString -> IO CString
c_realpath _ _ = throwIO (mkIOError UnsupportedOperation "platform does not support realpath" Nothing Nothing)

#else

foreign import ccall "realpath" c_realpath :: CString -> CString -> IO CString

#endif

withRealpath :: CString -> (CString -> IO a) -> IO a
withRealpath path action = case c_PATH_MAX of
  Nothing ->
    -- newer versions of POSIX support cases where the 2nd arg is NULL;
    -- hopefully that is the case here, as there is no safer way
    bracket (realpath nullPtr) c_free action
  Just pathMax ->
    -- allocate one extra just to be safe
    allocaBytes (pathMax + 1) (realpath >=> action)
  where realpath = throwErrnoIfNull "" . c_realpath path

realPath :: OsPath -> IO OsPath
realPath (OsString path') =
  Posix.withFilePath path'
    (`withRealpath` ((OsString <$>) . Posix.peekFilePath))

canonicalizePathSimplify :: OsPath -> IO OsPath
canonicalizePathSimplify = pure

findExecutablesLazyInternal :: ([OsPath] -> OsString -> ListT IO OsPath)
                            -> OsString
                            -> ListT IO OsPath
findExecutablesLazyInternal findExecutablesInDirectoriesLazy binary =
  liftJoinListT $ do
    path <- getPath
    pure (findExecutablesInDirectoriesLazy path binary)

exeExtensionInternal :: OsString
exeExtensionInternal = exeExtension

getDirectoryContentsInternal :: OsPath -> IO [OsPath]
getDirectoryContentsInternal (OsString path) =
  bracket
    (Posix.openDirStream path)
    Posix.closeDirStream
    start
  where
    start dirp = loop id
      where
        loop acc = do
          e <- Posix.readDirStream dirp
          if e == mempty
            then pure (acc [])
            else loop (acc . (OsString e :))

getCurrentDirectoryInternal :: IO OsPath
getCurrentDirectoryInternal = OsString <$> Posix.getWorkingDirectory

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
prependCurrentDirectory path
  | isRelative path =
    ((`ioeAddLocation` "prependCurrentDirectory") .
     (`ioeSetOsPath` path)) `modifyIOError` do
      (</> path) <$> getCurrentDirectoryInternal
  | otherwise = pure path

setCurrentDirectoryInternal :: OsPath -> IO ()
setCurrentDirectoryInternal = Posix.changeWorkingDirectory . getOsString

linkToDirectoryIsDirectory :: Bool
linkToDirectoryIsDirectory = False

createHardLink :: OsPath -> OsPath -> IO ()
createHardLink (OsString p1) (OsString p2) = Posix.createLink p1 p2

createSymbolicLink :: Bool -> OsPath -> OsPath -> IO ()
createSymbolicLink _ (OsString p1) (OsString p2) =
  Posix.createSymbolicLink p1 p2

readSymbolicLink :: OsPath -> IO OsPath
readSymbolicLink = (OsString <$>) . Posix.readSymbolicLink . getOsString

type Metadata = Posix.FileStatus

getSymbolicLinkMetadata :: OsPath -> IO Metadata
getSymbolicLinkMetadata = Posix.getSymbolicLinkStatus . getOsString

getFileMetadata :: OsPath -> IO Metadata
getFileMetadata = Posix.getFileStatus . getOsString

fileTypeFromMetadata :: Metadata -> FileType
fileTypeFromMetadata stat
  | isLink    = SymbolicLink
  | isDir     = Directory
  | otherwise = File
  where
    isLink = Posix.isSymbolicLink stat
    isDir  = Posix.isDirectory stat

fileSizeFromMetadata :: Metadata -> Integer
fileSizeFromMetadata = fromIntegral . Posix.fileSize

accessTimeFromMetadata :: Metadata -> UTCTime
accessTimeFromMetadata =
  POSIXTime.posixSecondsToUTCTime . Posix.accessTimeHiRes

modificationTimeFromMetadata :: Metadata -> UTCTime
modificationTimeFromMetadata =
  POSIXTime.posixSecondsToUTCTime . Posix.modificationTimeHiRes

type Mode = Posix.FileMode

modeFromMetadata :: Metadata -> Mode
modeFromMetadata = Posix.fileMode

allWriteMode :: Posix.FileMode
allWriteMode =
  Posix.ownerWriteMode .|.
  Posix.groupWriteMode .|.
  Posix.otherWriteMode

hasWriteMode :: Mode -> Bool
hasWriteMode m = m .&. allWriteMode /= 0

setWriteMode :: Bool -> Mode -> Mode
setWriteMode False m = m .&. complement allWriteMode
setWriteMode True  m = m .|. allWriteMode

setFileMode :: OsPath -> Mode -> IO ()
setFileMode = Posix.setFileMode . getOsString

setFilePermissions :: OsPath -> Mode -> IO ()
setFilePermissions = setFileMode

getAccessPermissions :: OsPath -> IO Permissions
getAccessPermissions path = do
  m <- getFileMetadata path
  let isDir = fileTypeIsDirectory (fileTypeFromMetadata m)
  let OsString path' = path
  r <- Posix.fileAccess path' True  False False
  w <- Posix.fileAccess path' False True  False
  x <- Posix.fileAccess path' False False True
  pure Permissions
       { readable   = r
       , writable   = w
       , executable = x && not isDir
       , searchable = x && isDir
       }

setAccessPermissions :: OsPath -> Permissions -> IO ()
setAccessPermissions path (Permissions r w e s) = do
  m <- getFileMetadata path
  setFileMode path (modifyBit (e || s) Posix.ownerExecuteMode .
                    modifyBit w Posix.ownerWriteMode .
                    modifyBit r Posix.ownerReadMode .
                    modeFromMetadata $ m)
  where
    modifyBit :: Bool -> Posix.FileMode -> Posix.FileMode -> Posix.FileMode
    modifyBit False b m = m .&. complement b
    modifyBit True  b m = m .|. b

copyOwnerFromStatus :: Posix.FileStatus -> OsPath -> IO ()
copyOwnerFromStatus st (OsString dst) = do
  Posix.setOwnerAndGroup dst (Posix.fileOwner st) (-1)

copyGroupFromStatus :: Posix.FileStatus -> OsPath -> IO ()
copyGroupFromStatus st (OsString dst) = do
  Posix.setOwnerAndGroup dst (-1) (Posix.fileGroup st)

tryCopyOwnerAndGroupFromStatus :: Posix.FileStatus -> OsPath -> IO ()
tryCopyOwnerAndGroupFromStatus st dst = do
  ignoreIOExceptions (copyOwnerFromStatus st dst)
  ignoreIOExceptions (copyGroupFromStatus st dst)

defaultFlags :: Posix.OpenFileFlags
defaultFlags =
  Posix.defaultFileFlags
  { Posix.noctty = True
  , Posix.nonBlock = True
  , Posix.cloexec = True
  }

openFileForRead :: OsPath -> IO Handle
openFileForRead (OsString p) =
  Posix.fdToHandle =<< Posix.openFd p Posix.ReadOnly defaultFlags

openFileForWrite :: OsPath -> IO Handle
openFileForWrite (OsString p) =
  Posix.fdToHandle =<<
    Posix.openFd p Posix.WriteOnly
      defaultFlags { Posix.creat = Just 0o666, Posix.trunc = True }

-- | Truncate the destination file and then copy the contents of the source
-- file to the destination file.  If the destination file already exists, its
-- attributes shall remain unchanged.  Otherwise, its attributes are reset to
-- the defaults.
copyFileContents :: OsPath              -- ^ Source filename
                 -> OsPath              -- ^ Destination filename
                 -> IO ()
copyFileContents fromFPath toFPath =
  (`ioeAddLocation` "copyFileContents") `modifyIOError` do
    withBinaryHandle (openFileForWrite toFPath) $ \ hTo -> do
      withBinaryHandle (openFileForRead fromFPath) $ \ hFrom -> do
        copyHandleData hFrom hTo

copyFileWithMetadataInternal :: (Metadata -> OsPath -> IO ())
                             -> (Metadata -> OsPath -> IO ())
                             -> OsPath
                             -> OsPath
                             -> IO ()
copyFileWithMetadataInternal copyPermissionsFromMetadata
                             copyTimesFromMetadata
                             src
                             dst = do
  st <- Posix.getFileStatus (getOsString src)
  copyFileContents src dst
  tryCopyOwnerAndGroupFromStatus st dst
  copyPermissionsFromMetadata st dst
  copyTimesFromMetadata st dst

setTimes :: OsPath -> (Maybe POSIXTime, Maybe POSIXTime) -> IO ()
#ifdef HAVE_UTIMENSAT
setTimes (OsString path') (atime', mtime') =
  Posix.withFilePath path' $ \ path'' ->
  withArray [ maybe utimeOmit toCTimeSpec atime'
            , maybe utimeOmit toCTimeSpec mtime' ] $ \ times ->
  Posix.throwErrnoPathIfMinus1_ "" path' $
    c_utimensat c_AT_FDCWD path'' times 0
#else
setTimes (OsString path') (Just atime', Just mtime') =
  Posix.setFileTimesHiRes path' atime' mtime'
setTimes (OsString path') (atime', mtime') = do
  m <- getFileMetadata (OsString path')
  let atimeOld = accessTimeFromMetadata m
  let mtimeOld = modificationTimeFromMetadata m
  Posix.setFileTimesHiRes path'
    (fromMaybe (POSIXTime.utcTimeToPOSIXSeconds atimeOld) atime')
    (fromMaybe (POSIXTime.utcTimeToPOSIXSeconds mtimeOld) mtime')
#endif

lookupEnvOs :: OsString -> IO (Maybe OsString)
lookupEnvOs (OsString name) = (OsString <$>) <$> Posix.getEnv name

getEnvOs :: OsString -> IO OsString
getEnvOs name = do
  env <- lookupEnvOs name
  case env of
    Nothing ->
      throwIO $
        mkIOError
          doesNotExistErrorType
          ("env var " <> show name <> " not found")
          Nothing
          Nothing
    Just value -> pure value

-- | Get the contents of the @PATH@ environment variable.
getPath :: IO [OsPath]
getPath = splitSearchPath <$> getEnvOs (os "PATH")

-- | $HOME is preferred, because the user has control over it. However, POSIX
-- doesn't define it as a mandatory variable, so fall back to `getpwuid_r`.
getHomeDirectoryInternal :: IO OsPath
getHomeDirectoryInternal = do
  e <- lookupEnvOs (os "HOME")
  case e of
       Just fp -> pure fp
       -- TODO: os here is bad, but unix's System.Posix.User.UserEntry does not
       -- have ByteString/OsString variants
       Nothing ->
         encodeFS =<<
         Posix.homeDirectory <$>
           (Posix.getEffectiveUserID >>= Posix.getUserEntryForID)

getXdgDirectoryFallback :: IO OsPath -> XdgDirectory -> IO OsPath
getXdgDirectoryFallback getHomeDirectory xdgDir = do
  (<$> getHomeDirectory) $ flip (</>) $ case xdgDir of
    XdgData   -> os ".local/share"
    XdgConfig -> os ".config"
    XdgCache  -> os ".cache"
    XdgState  -> os ".local/state"

getXdgDirectoryListFallback :: XdgDirectoryList -> IO [OsPath]
getXdgDirectoryListFallback xdgDirs =
  pure $ case xdgDirs of
    XdgDataDirs   -> [os "/usr/local/share/", os "/usr/share/"]
    XdgConfigDirs -> [os "/etc/xdg"]

getAppUserDataDirectoryInternal :: OsPath -> IO OsPath
getAppUserDataDirectoryInternal appName =
  (\ home -> home <> (os "/" <> os "." <> appName)) <$> getHomeDirectoryInternal

getUserDocumentsDirectoryInternal :: IO OsPath
getUserDocumentsDirectoryInternal = getHomeDirectoryInternal

getTemporaryDirectoryInternal :: IO OsPath
getTemporaryDirectoryInternal = fromMaybe (os "/tmp") <$> lookupEnvOs (os "TMPDIR")

#endif
