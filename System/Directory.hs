{-# LANGUAGE CPP, NondecreasingIndentation #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Directory
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- System-independent interface to directory manipulation.
--
-----------------------------------------------------------------------------

module System.Directory
   (
    -- $intro

    -- * Actions on directories
      createDirectory
    , createDirectoryIfMissing
    , removeDirectory
    , removeDirectoryRecursive
    , renameDirectory

    , getDirectoryContents
    , getCurrentDirectory
    , setCurrentDirectory

    -- * Pre-defined directories
    , getHomeDirectory
    , XdgDirectory(..)
    , getXdgDirectory
    , getAppUserDataDirectory
    , getUserDocumentsDirectory
    , getTemporaryDirectory

    -- * Actions on files
    , removeFile
    , renameFile
    , copyFile

    , canonicalizePath
    , makeAbsolute
    , makeRelativeToCurrentDirectory
    , findExecutable
    , findExecutables
    , findFile
    , findFiles
    , findFilesWith

    -- * Existence tests
    , doesFileExist
    , doesDirectoryExist

    -- * Permissions

    -- $permissions

    , Permissions
    , emptyPermissions
    , readable
    , writable
    , executable
    , searchable
    , setOwnerReadable
    , setOwnerWritable
    , setOwnerExecutable
    , setOwnerSearchable

    , getPermissions
    , setPermissions
    , copyPermissions

    -- * Timestamps

    , getAccessTime
    , getModificationTime
    , setAccessTime
    , setModificationTime

   ) where
import Control.Exception ( bracket, bracketOnError )
import Control.Monad ( when, unless )
#if !MIN_VERSION_base(4, 8, 0)
import Data.Functor ((<$>))
#endif
import Data.Maybe
  ( listToMaybe
#ifdef mingw32_HOST_OS
  , maybeToList
#endif
  )
import Data.Tuple (swap)

import System.FilePath
import System.IO
import System.IO.Error
  ( catchIOError
  , ioeSetErrorString
  , ioeSetFileName
  , ioeSetLocation
  , isAlreadyExistsError
  , isDoesNotExistError
  , isPermissionError
  , mkIOError
  , modifyIOError
  , tryIOError )

#ifdef __HUGS__
import Hugs.Directory
#endif /* __HUGS__ */

import Foreign

{-# CFILES cbits/directory.c #-}

import Data.Time ( UTCTime )
import Data.Time.Clock.POSIX
  ( posixSecondsToUTCTime
  , utcTimeToPOSIXSeconds
#ifdef mingw32_HOST_OS
  , POSIXTime
#endif
  )

#ifdef __GLASGOW_HASKELL__

import GHC.IO.Exception ( IOErrorType(InappropriateType) )

#ifdef mingw32_HOST_OS
import Foreign.C
import System.Posix.Types
import System.Posix.Internals
import qualified System.Win32 as Win32
#else
import GHC.IO.Encoding
import GHC.Foreign as GHC
import System.Environment ( getEnv )
import qualified System.Posix as Posix
#endif

#include <HsDirectoryConfig.h>
#ifdef HAVE_UTIMENSAT
import Foreign.C (throwErrnoPathIfMinus1_)
import System.Posix.Internals ( withFilePath )
#endif

#endif /* __GLASGOW_HASKELL__ */

import System.Directory.Internal

#ifdef mingw32_HOST_OS
win32_cSIDL_LOCAL_APPDATA :: Win32.CSIDL
win32_fILE_SHARE_DELETE   :: Win32.ShareMode
#if MIN_VERSION_Win32(2, 3, 1)
win32_cSIDL_LOCAL_APPDATA = Win32.cSIDL_LOCAL_APPDATA -- only on HEAD atm
win32_fILE_SHARE_DELETE   = Win32.fILE_SHARE_DELETE   -- added in 2.3.0.2
#else
win32_cSIDL_LOCAL_APPDATA = 0x001c
win32_fILE_SHARE_DELETE   = 0x00000004
#endif
#endif

{- $intro
A directory contains a series of entries, each of which is a named
reference to a file system object (file, directory etc.).  Some
entries may be hidden, inaccessible, or have some administrative
function (e.g. `.' or `..' under POSIX
<http://www.opengroup.org/onlinepubs/009695399/>), but in
this standard all such entries are considered to form part of the
directory contents. Entries in sub-directories are not, however,
considered to form part of the directory contents.

Each file system object is referenced by a /path/.  There is
normally at least one absolute path to each file system object.  In
some operating systems, it may also be possible to have paths which
are relative to the current directory.
-}

-----------------------------------------------------------------------------
-- Permissions

{- $permissions

 The 'Permissions' type is used to record whether certain operations are
 permissible on a file\/directory. 'getPermissions' and 'setPermissions'
 get and set these permissions, respectively. Permissions apply both to
 files and directories. For directories, the executable field will be
 'False', and for files the searchable field will be 'False'. Note that
 directories may be searchable without being readable, if permission has
 been given to use them as part of a path, but not to examine the
 directory contents.

Note that to change some, but not all permissions, a construct on the following lines must be used.

>  makeReadable f = do
>     p <- getPermissions f
>     setPermissions f (p {readable = True})

-}

data Permissions
 = Permissions {
    readable,   writable,
    executable, searchable :: Bool
   } deriving (Eq, Ord, Read, Show)

emptyPermissions :: Permissions
emptyPermissions = Permissions {
                       readable   = False,
                       writable   = False,
                       executable = False,
                       searchable = False
                   }

setOwnerReadable :: Bool -> Permissions -> Permissions
setOwnerReadable b p = p { readable = b }

setOwnerWritable :: Bool -> Permissions -> Permissions
setOwnerWritable b p = p { writable = b }

setOwnerExecutable :: Bool -> Permissions -> Permissions
setOwnerExecutable b p = p { executable = b }

setOwnerSearchable :: Bool -> Permissions -> Permissions
setOwnerSearchable b p = p { searchable = b }

{- |The 'getPermissions' operation returns the
permissions for the file or directory.

The operation may fail with:

* 'isPermissionError' if the user is not permitted to access
  the permissions; or

* 'isDoesNotExistError' if the file or directory does not exist.

-}

#ifdef __GLASGOW_HASKELL__

getPermissions :: FilePath -> IO Permissions
getPermissions name = do
#ifdef mingw32_HOST_OS
  -- issue #9: Windows doesn't like trailing path separators
  withFilePath (dropTrailingPathSeparator name) $ \s -> do
  -- stat() does a better job of guessing the permissions on Windows
  -- than access() does.  e.g. for execute permission, it looks at the
  -- filename extension :-)
  --
  -- I tried for a while to do this properly, using the Windows security API,
  -- and eventually gave up.  getPermissions is a flawed API anyway. -- SimonM
  allocaBytes sizeof_stat $ \ p_stat -> do
  throwErrnoIfMinus1_ "getPermissions" $ c_stat s p_stat
  mode <- st_mode p_stat
  let usr_read   = mode .&. s_IRUSR
  let usr_write  = mode .&. s_IWUSR
  let usr_exec   = mode .&. s_IXUSR
  let is_dir = mode .&. s_IFDIR
  return (
    Permissions {
      readable   = usr_read  /= 0,
      writable   = usr_write /= 0,
      executable = is_dir == 0 && usr_exec /= 0,
      searchable = is_dir /= 0 && usr_exec /= 0
    }
   )
#else
  read_ok  <- Posix.fileAccess name True  False False
  write_ok <- Posix.fileAccess name False True  False
  exec_ok  <- Posix.fileAccess name False False True
  stat <- Posix.getFileStatus name
  let is_dir = Posix.isDirectory stat
  return (
    Permissions {
      readable   = read_ok,
      writable   = write_ok,
      executable = not is_dir && exec_ok,
      searchable = is_dir && exec_ok
    }
   )
#endif

{- |The 'setPermissions' operation sets the
permissions for the file or directory.

The operation may fail with:

* 'isPermissionError' if the user is not permitted to set
  the permissions; or

* 'isDoesNotExistError' if the file or directory does not exist.

-}

setPermissions :: FilePath -> Permissions -> IO ()
setPermissions name (Permissions r w e s) = do
#ifdef mingw32_HOST_OS
  allocaBytes sizeof_stat $ \ p_stat -> do
  withFilePath name $ \p_name -> do
    throwErrnoIfMinus1_ "setPermissions" $
      c_stat p_name p_stat

    throwErrnoIfMinus1_ "setPermissions" $ do
      mode <- st_mode p_stat
      let mode1 = modifyBit r mode s_IRUSR
      let mode2 = modifyBit w mode1 s_IWUSR
      let mode3 = modifyBit (e || s) mode2 s_IXUSR
      c_wchmod p_name mode3
 where
   modifyBit :: Bool -> CMode -> CMode -> CMode
   modifyBit False m b = m .&. (complement b)
   modifyBit True  m b = m .|. b
#else
      stat <- Posix.getFileStatus name
      let mode = Posix.fileMode stat
      let mode1 = modifyBit r mode  Posix.ownerReadMode
      let mode2 = modifyBit w mode1 Posix.ownerWriteMode
      let mode3 = modifyBit (e || s) mode2 Posix.ownerExecuteMode
      Posix.setFileMode name mode3
 where
   modifyBit :: Bool -> Posix.FileMode -> Posix.FileMode -> Posix.FileMode
   modifyBit False m b = m .&. (complement b)
   modifyBit True  m b = m .|. b
#endif

#ifdef mingw32_HOST_OS
foreign import ccall unsafe "_wchmod"
   c_wchmod :: CWString -> CMode -> IO CInt
#endif

copyPermissions :: FilePath -> FilePath -> IO ()
copyPermissions source dest = do
#ifdef mingw32_HOST_OS
  allocaBytes sizeof_stat $ \ p_stat -> do
  withFilePath source $ \p_source -> do
  withFilePath dest $ \p_dest -> do
    throwErrnoIfMinus1_ "copyPermissions" $ c_stat p_source p_stat
    mode <- st_mode p_stat
    throwErrnoIfMinus1_ "copyPermissions" $ c_wchmod p_dest mode
#else
  stat <- Posix.getFileStatus source
  let mode = Posix.fileMode stat
  Posix.setFileMode dest mode
#endif

-----------------------------------------------------------------------------
-- Implementation

{- |@'createDirectory' dir@ creates a new directory @dir@ which is
initially empty, or as near to empty as the operating system
allows.

The operation may fail with:

* 'isPermissionError' \/ 'PermissionDenied'
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES]@

* 'isAlreadyExistsError' \/ 'AlreadyExists'
The operand refers to a directory that already exists.
@ [EEXIST]@

* 'HardwareFault'
A physical I\/O error has occurred.
@[EIO]@

* 'InvalidArgument'
The operand is not a valid directory name.
@[ENAMETOOLONG, ELOOP]@

* 'NoSuchThing'
There is no path to the directory.
@[ENOENT, ENOTDIR]@

* 'ResourceExhausted'
Insufficient resources (virtual memory, process file descriptors,
physical disk space, etc.) are available to perform the operation.
@[EDQUOT, ENOSPC, ENOMEM, EMLINK]@

* 'InappropriateType'
The path refers to an existing non-directory object.
@[EEXIST]@

-}

createDirectory :: FilePath -> IO ()
createDirectory path = do
#ifdef mingw32_HOST_OS
  Win32.createDirectory path Nothing
#else
  Posix.createDirectory path 0o777
#endif

#else /* !__GLASGOW_HASKELL__ */

copyPermissions :: FilePath -> FilePath -> IO ()
copyPermissions fromFPath toFPath
  = getPermissions fromFPath >>= setPermissions toFPath

#endif

-- | @'createDirectoryIfMissing' parents dir@ creates a new directory
-- @dir@ if it doesn\'t exist. If the first argument is 'True'
-- the function will also create all parent directories if they are missing.
createDirectoryIfMissing :: Bool     -- ^ Create its parents too?
                         -> FilePath -- ^ The path to the directory you want to make
                         -> IO ()
createDirectoryIfMissing create_parents path0
  | create_parents = createDirs (parents path0)
  | otherwise      = createDirs (take 1 (parents path0))
  where
    parents = reverse . scanl1 (</>) . splitDirectories . normalise

    createDirs []         = return ()
    createDirs (dir:[])   = createDir dir ioError
    createDirs (dir:dirs) =
      createDir dir $ \_ -> do
        createDirs dirs
        createDir dir ioError

    createDir dir notExistHandler = do
      r <- tryIOError (createDirectory dir)
      case r of
        Right ()                   -> return ()
        Left  e
          | isDoesNotExistError  e -> notExistHandler e
          -- createDirectory (and indeed POSIX mkdir) does not distinguish
          -- between a dir already existing and a file already existing. So we
          -- check for it here. Unfortunately there is a slight race condition
          -- here, but we think it is benign. It could report an exeption in
          -- the case that the dir did exist but another process deletes the
          -- directory and creates a file in its place before we can check
          -- that the directory did indeed exist.
          -- We also follow this path when we get a permissions error, as
          -- trying to create "." when in the root directory on Windows
          -- fails with
          --     CreateDirectory ".": permission denied (Access is denied.)
          -- This caused GHCi to crash when loading a module in the root
          -- directory.
          | isAlreadyExistsError e
         || isPermissionError    e -> do
              canIgnore <- isDir `catchIOError` \ _ ->
                           return (isAlreadyExistsError e)
              unless canIgnore (ioError e)
          | otherwise              -> ioError e
      where
#ifdef mingw32_HOST_OS
        isDir = withFileStatus "createDirectoryIfMissing" dir isDirectory
#else
        isDir = (Posix.isDirectory <$> Posix.getFileStatus dir)
#endif

#if __GLASGOW_HASKELL__

-- | * @'NotDirectory'@:   not a directory.
--   * @'Directory'@:      a true directory (not a symbolic link).
--   * @'DirectoryLink'@:  a directory symbolic link (only exists on Windows).
data DirectoryType = NotDirectory
                   | Directory
                   | DirectoryLink
                   deriving (Enum, Eq, Ord, Read, Show)

-- | Obtain the type of a directory.
getDirectoryType :: FilePath -> IO DirectoryType
getDirectoryType path =
  (`ioeSetLocation` "getDirectoryType") `modifyIOError` do
#ifdef mingw32_HOST_OS
    classify <$> Win32.getFileAttributes path
    where fILE_ATTRIBUTE_REPARSE_POINT = 0x400
          classify attr
            | attr .&. Win32.fILE_ATTRIBUTE_DIRECTORY == 0 = NotDirectory
            | attr .&. fILE_ATTRIBUTE_REPARSE_POINT   == 0 = Directory
            | otherwise                                    = DirectoryLink
#else
    stat <- Posix.getSymbolicLinkStatus path
    return $ if Posix.isDirectory stat
             then Directory
             else NotDirectory
#endif

{- | @'removeDirectory' dir@ removes an existing directory /dir/.  The
implementation may specify additional constraints which must be
satisfied before a directory can be removed (e.g. the directory has to
be empty, or may not be in use by other processes).  It is not legal
for an implementation to partially remove a directory unless the
entire directory is removed. A conformant implementation need not
support directory removal in all situations (e.g. removal of the root
directory).

The operation may fail with:

* 'HardwareFault'
A physical I\/O error has occurred.
@[EIO]@

* 'InvalidArgument'
The operand is not a valid directory name.
@[ENAMETOOLONG, ELOOP]@

* 'isDoesNotExistError' \/ 'NoSuchThing'
The directory does not exist.
@[ENOENT, ENOTDIR]@

* 'isPermissionError' \/ 'PermissionDenied'
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES, EPERM]@

* 'UnsatisfiedConstraints'
Implementation-dependent constraints are not satisfied.
@[EBUSY, ENOTEMPTY, EEXIST]@

* 'UnsupportedOperation'
The implementation does not support removal in this situation.
@[EINVAL]@

* 'InappropriateType'
The operand refers to an existing non-directory object.
@[ENOTDIR]@

-}

removeDirectory :: FilePath -> IO ()
removeDirectory path =
#ifdef mingw32_HOST_OS
  Win32.removeDirectory path
#else
  Posix.removeDirectory path
#endif

#endif

-- | @'removeDirectoryRecursive' dir@ removes an existing directory /dir/
-- together with its contents and subdirectories. Within this directory,
-- symbolic links are removed without affecting their the targets.
removeDirectoryRecursive :: FilePath -> IO ()
removeDirectoryRecursive path =
  (`ioeSetLocation` "removeDirectoryRecursive") `modifyIOError` do
    dirType <- getDirectoryType path
    case dirType of
      Directory -> removeContentsRecursive path
      _         -> ioError . (`ioeSetErrorString` "not a directory") $
                   mkIOError InappropriateType "" Nothing (Just path)

-- | @'removePathRecursive' path@ removes an existing file or directory at
-- /path/ together with its contents and subdirectories. Symbolic links are
-- removed without affecting their the targets.
removePathRecursive :: FilePath -> IO ()
removePathRecursive path =
  (`ioeSetLocation` "removePathRecursive") `modifyIOError` do
    dirType <- getDirectoryType path
    case dirType of
      NotDirectory  -> removeFile path
      Directory     -> removeContentsRecursive path
      DirectoryLink -> removeDirectory path

-- | @'removeContentsRecursive' dir@ removes the contents of the directory
-- /dir/ recursively. Symbolic links are removed without affecting their the
-- targets.
removeContentsRecursive :: FilePath -> IO ()
removeContentsRecursive path =
  (`ioeSetLocation` "removeContentsRecursive") `modifyIOError` do
    cont <- getDirectoryContents path
    mapM_ removePathRecursive [path </> x | x <- cont, x /= "." && x /= ".."]
    removeDirectory path

#if __GLASGOW_HASKELL__
{- |'removeFile' /file/ removes the directory entry for an existing file
/file/, where /file/ is not itself a directory. The
implementation may specify additional constraints which must be
satisfied before a file can be removed (e.g. the file may not be in
use by other processes).

The operation may fail with:

* 'HardwareFault'
A physical I\/O error has occurred.
@[EIO]@

* 'InvalidArgument'
The operand is not a valid file name.
@[ENAMETOOLONG, ELOOP]@

* 'isDoesNotExistError' \/ 'NoSuchThing'
The file does not exist.
@[ENOENT, ENOTDIR]@

* 'isPermissionError' \/ 'PermissionDenied'
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES, EPERM]@

* 'UnsatisfiedConstraints'
Implementation-dependent constraints are not satisfied.
@[EBUSY]@

* 'InappropriateType'
The operand refers to an existing directory.
@[EPERM, EINVAL]@

-}

removeFile :: FilePath -> IO ()
removeFile path =
#if mingw32_HOST_OS
  Win32.deleteFile path
#else
  Posix.removeLink path
#endif

{- |@'renameDirectory' old new@ changes the name of an existing
directory from /old/ to /new/.  If the /new/ directory
already exists, it is atomically replaced by the /old/ directory.
If the /new/ directory is neither the /old/ directory nor an
alias of the /old/ directory, it is removed as if by
'removeDirectory'.  A conformant implementation need not support
renaming directories in all situations (e.g. renaming to an existing
directory, or across different physical devices), but the constraints
must be documented.

On Win32 platforms, @renameDirectory@ fails if the /new/ directory already
exists.

The operation may fail with:

* 'HardwareFault'
A physical I\/O error has occurred.
@[EIO]@

* 'InvalidArgument'
Either operand is not a valid directory name.
@[ENAMETOOLONG, ELOOP]@

* 'isDoesNotExistError' \/ 'NoSuchThing'
The original directory does not exist, or there is no path to the target.
@[ENOENT, ENOTDIR]@

* 'isPermissionError' \/ 'PermissionDenied'
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES, EPERM]@

* 'ResourceExhausted'
Insufficient resources are available to perform the operation.
@[EDQUOT, ENOSPC, ENOMEM, EMLINK]@

* 'UnsatisfiedConstraints'
Implementation-dependent constraints are not satisfied.
@[EBUSY, ENOTEMPTY, EEXIST]@

* 'UnsupportedOperation'
The implementation does not support renaming in this situation.
@[EINVAL, EXDEV]@

* 'InappropriateType'
Either path refers to an existing non-directory object.
@[ENOTDIR, EISDIR]@

-}

renameDirectory :: FilePath -> FilePath -> IO ()
renameDirectory opath npath = do
   -- XXX this test isn't performed atomically with the following rename
#ifdef mingw32_HOST_OS
   -- ToDo: use Win32 API
   withFileStatus "renameDirectory" opath $ \st -> do
   is_dir <- isDirectory st
#else
   stat <- Posix.getFileStatus opath
   let is_dir = Posix.fileMode stat .&. Posix.directoryMode /= 0
#endif
   if (not is_dir)
        then ioError (ioeSetErrorString
                          (mkIOError InappropriateType "renameDirectory" Nothing (Just opath))
                          "not a directory")
        else do
#ifdef mingw32_HOST_OS
   Win32.moveFileEx opath npath Win32.mOVEFILE_REPLACE_EXISTING
#else
   Posix.rename opath npath
#endif

{- |@'renameFile' old new@ changes the name of an existing file system
object from /old/ to /new/.  If the /new/ object already
exists, it is atomically replaced by the /old/ object.  Neither
path may refer to an existing directory.  A conformant implementation
need not support renaming files in all situations (e.g. renaming
across different physical devices), but the constraints must be
documented.

The operation may fail with:

* 'HardwareFault'
A physical I\/O error has occurred.
@[EIO]@

* 'InvalidArgument'
Either operand is not a valid file name.
@[ENAMETOOLONG, ELOOP]@

* 'isDoesNotExistError' \/ 'NoSuchThing'
The original file does not exist, or there is no path to the target.
@[ENOENT, ENOTDIR]@

* 'isPermissionError' \/ 'PermissionDenied'
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES, EPERM]@

* 'ResourceExhausted'
Insufficient resources are available to perform the operation.
@[EDQUOT, ENOSPC, ENOMEM, EMLINK]@

* 'UnsatisfiedConstraints'
Implementation-dependent constraints are not satisfied.
@[EBUSY]@

* 'UnsupportedOperation'
The implementation does not support renaming in this situation.
@[EXDEV]@

* 'InappropriateType'
Either path refers to an existing directory.
@[ENOTDIR, EISDIR, EINVAL, EEXIST, ENOTEMPTY]@

-}

renameFile :: FilePath -> FilePath -> IO ()
renameFile opath npath = (`ioeSetLocation` "renameFile") `modifyIOError` do
   -- XXX the tests are not performed atomically with the rename
   checkNotDir opath
#ifdef mingw32_HOST_OS
   Win32.moveFileEx opath npath Win32.mOVEFILE_REPLACE_EXISTING
#else
   Posix.rename opath npath
#endif
     -- The underlying rename implementation can throw odd exceptions when the
     -- destination is a directory.  For example, Windows typically throws a
     -- permission error, while POSIX systems may throw a resource busy error
     -- if one of the paths refers to the current directory.  In these cases,
     -- we check if the destination is a directory and, if so, throw an
     -- InappropriateType error.
     `catchIOError` \ err -> do
       checkNotDir npath
       ioError err
   where checkNotDir path = do
           dirType <- getDirectoryType path
                      `catchIOError` \ _ -> return NotDirectory
           case dirType of
             Directory     -> errIsDir path
             DirectoryLink -> errIsDir path
             NotDirectory  -> return ()
         errIsDir path = ioError . (`ioeSetErrorString` "is a directory") $
                         mkIOError InappropriateType "" Nothing (Just path)

#endif /* __GLASGOW_HASKELL__ */

{- |@'copyFile' old new@ copies the existing file from /old/ to /new/.
If the /new/ file already exists, it is atomically replaced by the /old/ file.
Neither path may refer to an existing directory.  The permissions of /old/ are
copied to /new/, if possible.
-}

copyFile :: FilePath -> FilePath -> IO ()
copyFile fromFPath toFPath =
    copy `catchIOError` (\ exc -> ioError (ioeSetLocation exc "copyFile"))
    where copy = bracket (openBinaryFile fromFPath ReadMode) hClose $ \hFrom ->
                 bracketOnError openTmp cleanTmp $ \(tmpFPath, hTmp) ->
                 do allocaBytes bufferSize $ copyContents hFrom hTmp
                    hClose hTmp
                    ignoreIOExceptions $ copyPermissions fromFPath tmpFPath
                    renameFile tmpFPath toFPath
          openTmp = openBinaryTempFile (takeDirectory toFPath) ".copyFile.tmp"
          cleanTmp (tmpFPath, hTmp)
              = do ignoreIOExceptions $ hClose hTmp
                   ignoreIOExceptions $ removeFile tmpFPath
          bufferSize = 1024

          copyContents hFrom hTo buffer = do
                  count <- hGetBuf hFrom buffer bufferSize
                  when (count > 0) $ do
                          hPutBuf hTo buffer count
                          copyContents hFrom hTo buffer

          ignoreIOExceptions io = io `catchIOError` (\_ -> return ())

-- | Make a path absolute and remove as many indirections from it as possible.
-- Indirections include the two special directories @.@ and @..@, as well as
-- any symbolic links.  The input path need not point to an existing file or
-- directory.
--
-- __Note__: if you require only an absolute path, use 'makeAbsolute' instead.
-- Most programs need not care about whether a path contains symbolic links.
--
-- Due to the fact that symbolic links and @..@ are dependent on the state of
-- the existing filesystem, the function can only make a conservative,
-- best-effort attempt.  Nevertheless, if the input path points to an existing
-- file or directory, then the output path shall also point to the same file
-- or directory.
--
-- Formally, symbolic links and @..@ are removed from the longest prefix of
-- the path that still points to an existing file.  The function is not
-- atomic, therefore concurrent changes in the filesystem may lead to
-- incorrect results.
--
-- (Despite the name, the function does not guarantee canonicity of the
-- returned path due to the presence of hard links, mount points, etc.)
--
-- Similar to 'normalise', an empty path is equivalent to the current
-- directory.
--
-- /Known bug(s)/: on Windows, the function does not resolve symbolic links.
--
-- /Changes since 1.2.3.0:/ The function has been altered to be more robust
-- and has the same exception behavior as 'makeAbsolute'.
--
canonicalizePath :: FilePath -> IO FilePath
canonicalizePath = \ path ->
  modifyIOError ((`ioeSetLocation` "canonicalizePath") .
                 (`ioeSetFileName` path)) $
  -- normalise does more stuff, like upper-casing the drive letter
  normalise <$> (transform =<< makeAbsolute path)
  where
#if defined(mingw32_HOST_OS)
    transform path = Win32.getFullPathName path
                     `catchIOError` \ _ -> return path
#else
    transform path = copySlash path <$> do
      encoding <- getFileSystemEncoding
      realpathPrefix encoding (reverse (zip prefixes suffixes)) path
      where segments = splitPath path
            prefixes = scanl1 (</>) segments
            suffixes = tail (scanr (</>) "" segments)

    -- call realpath on the largest possible prefix
    realpathPrefix encoding ((prefix, suffix) : rest) path = do
      exist <- doesPathExist prefix
      if exist -- never call realpath on an inaccessible path
        then ((</> suffix) <$> realpath encoding prefix)
             `catchIOError` \ _ -> realpathPrefix encoding rest path
        else realpathPrefix encoding rest path
    realpathPrefix _ _ path = return path

    realpath encoding path =
      GHC.withCString encoding path
      (`withRealpath` GHC.peekCString encoding)

    doesPathExist path = (Posix.getFileStatus path >> return True)
                         `catchIOError` \ _ -> return False

    -- make sure trailing slash is preserved
    copySlash path | hasTrailingPathSeparator path = addTrailingPathSeparator
                   | otherwise                     = id
#endif

-- | Make a path absolute by prepending the current directory (if it isn't
-- already absolute) and applying 'normalise' to the result.
--
-- If the path is already absolute, the operation never fails.  Otherwise, the
-- operation may fail with the same exceptions as 'getCurrentDirectory'.
--
-- @since 1.2.2.0
makeAbsolute :: FilePath -> IO FilePath
makeAbsolute = (normalise <$>) . absolutize
  where absolutize path -- avoid the call to `getCurrentDirectory` if we can
          | isRelative path = (</> path) . addTrailingPathSeparator <$>
                              getCurrentDirectory
          | otherwise       = return path

-- | 'makeRelative' the current directory.
makeRelativeToCurrentDirectory :: FilePath -> IO FilePath
makeRelativeToCurrentDirectory x = do
    cur <- getCurrentDirectory
    return $ makeRelative cur x

-- | Given an executable file name, searches for such file in the
-- directories listed in system PATH. The returned value is the path
-- to the found executable or Nothing if an executable with the given
-- name was not found. For example (findExecutable \"ghc\") gives you
-- the path to GHC.
--
-- The path returned by 'findExecutable' corresponds to the
-- program that would be executed by 'System.Process.createProcess'
-- when passed the same string (as a RawCommand, not a ShellCommand).
--
-- On Windows, 'findExecutable' calls the Win32 function 'SearchPath',
-- which may search other places before checking the directories in
-- @PATH@.  Where it actually searches depends on registry settings,
-- but notably includes the directory containing the current
-- executable. See
-- <http://msdn.microsoft.com/en-us/library/aa365527.aspx> for more
-- details.
--
findExecutable :: String -> IO (Maybe FilePath)
findExecutable fileName = do
   files <- findExecutables fileName
   return $ listToMaybe files

-- | Given a file name, searches for the file and returns a list of all
-- occurences that are executable.
--
-- @since 1.2.2.0
findExecutables :: String -> IO [FilePath]
findExecutables binary = do
#if defined(mingw32_HOST_OS)
    file <- Win32.searchPath Nothing binary exeExtension
    return $ maybeToList file
#else
    path <- getEnv "PATH"
    findFilesWith isExecutable (splitSearchPath path) (binary <.> exeExtension)
  where isExecutable file = do
            perms <- getPermissions file
            return $ executable perms
#endif

-- | Search through the given set of directories for the given file.
-- Used by 'findExecutable' on non-windows platforms.
findFile :: [FilePath] -> String -> IO (Maybe FilePath)
findFile path fileName = do
    files <- findFiles path fileName
    return $ listToMaybe files

-- | Search through the given set of directories for the given file and
-- returns a list of paths where the given file exists.
--
-- @since 1.2.1.0
findFiles :: [FilePath] -> String -> IO [FilePath]
findFiles = findFilesWith (\_ -> return True)

-- | Search through the given set of directories for the given file and
-- with the given property (usually permissions) and returns a list of
-- paths where the given file exists and has the property.
--
-- @since 1.2.1.0
findFilesWith :: (FilePath -> IO Bool) -> [FilePath] -> String -> IO [FilePath]
findFilesWith _ [] _ = return []
findFilesWith f (d:ds) fileName = do
    let file = d </> fileName
    exist <- doesFileExist file
    b <- if exist then f file else return False
    if b then do
               files <- findFilesWith f ds fileName
               return $ file : files
        else findFilesWith f ds fileName

#ifdef __GLASGOW_HASKELL__
{- |@'getDirectoryContents' dir@ returns a list of /all/ entries
in /dir/.

The operation may fail with:

* 'HardwareFault'
A physical I\/O error has occurred.
@[EIO]@

* 'InvalidArgument'
The operand is not a valid directory name.
@[ENAMETOOLONG, ELOOP]@

* 'isDoesNotExistError' \/ 'NoSuchThing'
The directory does not exist.
@[ENOENT, ENOTDIR]@

* 'isPermissionError' \/ 'PermissionDenied'
The process has insufficient privileges to perform the operation.
@[EACCES]@

* 'ResourceExhausted'
Insufficient resources are available to perform the operation.
@[EMFILE, ENFILE]@

* 'InappropriateType'
The path refers to an existing non-directory object.
@[ENOTDIR]@

-}

getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents path =
  modifyIOError ((`ioeSetFileName` path) .
                 (`ioeSetLocation` "getDirectoryContents")) $ do
#ifndef mingw32_HOST_OS
    bracket
      (Posix.openDirStream path)
      Posix.closeDirStream
      start
 where
  start dirp =
      loop id
    where
      loop acc = do
        e <- Posix.readDirStream dirp
        if null e
          then return (acc [])
          else loop (acc . (e:))
#else
  bracket
     (Win32.findFirstFile (path </> "*"))
     (\(h,_) -> Win32.findClose h)
     (\(h,fdat) -> loop h fdat [])
  where
        -- we needn't worry about empty directories: adirectory always
        -- has at least "." and ".." entries
    loop :: Win32.HANDLE -> Win32.FindData -> [FilePath] -> IO [FilePath]
    loop h fdat acc = do
       filename <- Win32.getFindDataFileName fdat
       more <- Win32.findNextFile h fdat
       if more
          then loop h fdat (filename:acc)
          else return (filename:acc)
                 -- no need to reverse, ordering is undefined
#endif /* mingw32 */

#endif /* __GLASGOW_HASKELL__ */


{- |If the operating system has a notion of current directories,
'getCurrentDirectory' returns an absolute path to the
current directory of the calling process.

The operation may fail with:

* 'HardwareFault'
A physical I\/O error has occurred.
@[EIO]@

* 'isDoesNotExistError' \/ 'NoSuchThing'
There is no path referring to the current directory.
@[EPERM, ENOENT, ESTALE...]@

* 'isPermissionError' \/ 'PermissionDenied'
The process has insufficient privileges to perform the operation.
@[EACCES]@

* 'ResourceExhausted'
Insufficient resources are available to perform the operation.

* 'UnsupportedOperation'
The operating system has no notion of current directory.

Note that in a concurrent program, the current directory is global
state shared between all threads of the process.  When using
filesystem operations from multiple threads, it is therefore highly
recommended to use absolute rather than relative `FilePath`s.

-}
#ifdef __GLASGOW_HASKELL__
getCurrentDirectory :: IO FilePath
getCurrentDirectory = do
#ifdef mingw32_HOST_OS
  Win32.getCurrentDirectory
#else
  Posix.getWorkingDirectory
#endif

{- |If the operating system has a notion of current directories,
@'setCurrentDirectory' dir@ changes the current
directory of the calling process to /dir/.

The operation may fail with:

* 'HardwareFault'
A physical I\/O error has occurred.
@[EIO]@

* 'InvalidArgument'
The operand is not a valid directory name.
@[ENAMETOOLONG, ELOOP]@

* 'isDoesNotExistError' \/ 'NoSuchThing'
The directory does not exist.
@[ENOENT, ENOTDIR]@

* 'isPermissionError' \/ 'PermissionDenied'
The process has insufficient privileges to perform the operation.
@[EACCES]@

* 'UnsupportedOperation'
The operating system has no notion of current directory, or the
current directory cannot be dynamically changed.

* 'InappropriateType'
The path refers to an existing non-directory object.
@[ENOTDIR]@

Note that in a concurrent program, the current directory is global
state shared between all threads of the process.  When using
filesystem operations from multiple threads, it is therefore highly
recommended to use absolute rather than relative `FilePath`s.

-}

setCurrentDirectory :: FilePath -> IO ()
setCurrentDirectory path =
#ifdef mingw32_HOST_OS
  Win32.setCurrentDirectory path
#else
  Posix.changeWorkingDirectory path
#endif

{- |The operation 'doesDirectoryExist' returns 'True' if the argument file
exists and is either a directory or a symbolic link to a directory,
and 'False' otherwise.
-}

doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist name =
#ifdef mingw32_HOST_OS
   (withFileStatus "doesDirectoryExist" name $ \st -> isDirectory st)
#else
   (do stat <- Posix.getFileStatus name
       return (Posix.isDirectory stat))
#endif
   `catchIOError` \ _ -> return False

{- |The operation 'doesFileExist' returns 'True'
if the argument file exists and is not a directory, and 'False' otherwise.
-}

doesFileExist :: FilePath -> IO Bool
doesFileExist name =
#ifdef mingw32_HOST_OS
   (withFileStatus "doesFileExist" name $ \st -> do b <- isDirectory st; return (not b))
#else
   (do stat <- Posix.getFileStatus name
       return (not (Posix.isDirectory stat)))
#endif
   `catchIOError` \ _ -> return False

#ifdef mingw32_HOST_OS
-- | Open the handle of an existing file or directory.
openFileHandle :: String -> Win32.AccessMode -> IO Win32.HANDLE
openFileHandle path mode = Win32.createFile path mode share Nothing
                                            Win32.oPEN_EXISTING flags Nothing
  where share =  win32_fILE_SHARE_DELETE
             .|. Win32.fILE_SHARE_READ
             .|. Win32.fILE_SHARE_WRITE
        flags =  Win32.fILE_ATTRIBUTE_NORMAL
             .|. Win32.fILE_FLAG_BACKUP_SEMANTICS -- required for directories
#endif

-- | Obtain the time at which the file or directory was last accessed.
--
-- The operation may fail with:
--
-- * 'isPermissionError' if the user is not permitted to read
--   the access time; or
--
-- * 'isDoesNotExistError' if the file or directory does not exist.
--
-- Caveat for POSIX systems: This function returns a timestamp with sub-second
-- resolution only if this package is compiled against @unix-2.6.0.0@ or later
-- and the underlying filesystem supports them.
--
-- @since 1.2.3.0
--
getAccessTime :: FilePath -> IO UTCTime
getAccessTime = modifyIOError (`ioeSetLocation` "getAccessTime") .
                getFileTime False

-- | Obtain the time at which the file or directory was last modified.
--
-- The operation may fail with:
--
-- * 'isPermissionError' if the user is not permitted to read
--   the modification time; or
--
-- * 'isDoesNotExistError' if the file or directory does not exist.
--
-- Caveat for POSIX systems: This function returns a timestamp with sub-second
-- resolution only if this package is compiled against @unix-2.6.0.0@ or later
-- and the underlying filesystem supports them.
--
getModificationTime :: FilePath -> IO UTCTime
getModificationTime = modifyIOError (`ioeSetLocation` "getModificationTime") .
                      getFileTime True

getFileTime :: Bool -> FilePath -> IO UTCTime
getFileTime isMtime path = modifyIOError (`ioeSetFileName` path) $
                           posixSecondsToUTCTime <$> getTime
  where
    path' = normalise path              -- handle empty paths
#ifdef mingw32_HOST_OS
    getTime =
      bracket (openFileHandle path' Win32.gENERIC_READ)
              Win32.closeHandle $ \ handle ->
      alloca $ \ time -> do
        Win32.failIf_ not "" .
          uncurry (Win32.c_GetFileTime handle nullPtr) $
           swapIf isMtime (time, nullPtr)
        windowsToPosixTime <$> peek time
#else
    getTime = convertTime <$> Posix.getFileStatus path'
# if MIN_VERSION_unix(2, 6, 0)
    convertTime = if isMtime then Posix.modificationTimeHiRes
                             else Posix.accessTimeHiRes
# else
    convertTime = realToFrac . if isMtime then Posix.modificationTime
                                          else Posix.accessTime
# endif
#endif

-- | Change the time at which the file or directory was last accessed.
--
-- The operation may fail with:
--
-- * 'isPermissionError' if the user is not permitted to alter the
--   access time; or
--
-- * 'isDoesNotExistError' if the file or directory does not exist.
--
-- Some caveats for POSIX systems:
--
-- * Not all systems support @utimensat@, in which case the function can only
--   emulate the behavior by reading the modification time and then setting
--   both the access and modification times together.  On systems where
--   @utimensat@ is supported, the access time is set atomically with
--   nanosecond precision.
--
-- * If compiled against a version of @unix@ prior to @2.7.0.0@, the function
--   would not be able to set timestamps with sub-second resolution.  In this
--   case, there would also be loss of precision in the modification time.
--
-- @since 1.2.3.0
--
setAccessTime :: FilePath -> UTCTime -> IO ()
setAccessTime path =
  modifyIOError (`ioeSetLocation` "setAccessTime") .
  setFileTime False path

-- | Change the time at which the file or directory was last modified.
--
-- The operation may fail with:
--
-- * 'isPermissionError' if the user is not permitted to alter the
--   modification time; or
--
-- * 'isDoesNotExistError' if the file or directory does not exist.
--
-- Some caveats for POSIX systems:
--
-- * Not all systems support @utimensat@, in which case the function can only
--   emulate the behavior by reading the access time and then setting both the
--   access and modification times together.  On systems where @utimensat@ is
--   supported, the modification time is set atomically with nanosecond
--   precision.
--
-- * If compiled against a version of @unix@ prior to @2.7.0.0@, the function
--   would not be able to set timestamps with sub-second resolution.  In this
--   case, there would also be loss of precision in the access time.
--
-- @since 1.2.3.0
--
setModificationTime :: FilePath -> UTCTime -> IO ()
setModificationTime path =
  modifyIOError (`ioeSetLocation` "setModificationTime") .
  setFileTime True path

setFileTime :: Bool -> FilePath -> UTCTime -> IO ()
setFileTime isMtime path = modifyIOError (`ioeSetFileName` path) .
                           setTime . utcTimeToPOSIXSeconds
  where
    path'  = normalise path             -- handle empty paths
#ifdef mingw32_HOST_OS
    setTime time =
      bracket (openFileHandle path' Win32.gENERIC_WRITE)
              Win32.closeHandle $ \ handle ->
      with (posixToWindowsTime time) $ \ time' ->
      Win32.failIf_ not "" .
        uncurry (Win32.c_SetFileTime handle nullPtr) $
          swapIf isMtime (time', nullPtr)
#elif defined HAVE_UTIMENSAT
    setTime time =
      withFilePath path' $ \ path'' ->
      withArray [atime, mtime] $ \ times ->
      throwErrnoPathIfMinus1_ "" path' $
      c_utimensat c_AT_FDCWD path'' times 0
      where (atime, mtime) = swapIf isMtime (toCTimeSpec time, utimeOmit)
#else
    setTime time = do
      stat <- Posix.getFileStatus path'
      uncurry (setFileTimes path') $
        swapIf isMtime (convertTime time, otherTime stat)
# if MIN_VERSION_unix(2, 7, 0)
    setFileTimes = Posix.setFileTimesHiRes
    convertTime  = id
    otherTime    = if isMtime
                   then Posix.accessTimeHiRes
                   else Posix.modificationTimeHiRes
#  else
    setFileTimes = Posix.setFileTimes
    convertTime  = fromInteger . truncate
    otherTime    = if isMtime
                   then Posix.accessTime
                   else Posix.modificationTime
# endif
#endif

swapIf :: Bool -> (a, a) -> (a, a)
swapIf True  = swap
swapIf False = id

#ifdef mingw32_HOST_OS
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
#endif

#endif /* __GLASGOW_HASKELL__ */

#ifdef mingw32_HOST_OS
withFileStatus :: String -> FilePath -> (Ptr CStat -> IO a) -> IO a
withFileStatus loc name f = do
  modifyIOError (`ioeSetFileName` name) $
    allocaBytes sizeof_stat $ \p ->
      withFilePath (fileNameEndClean name) $ \s -> do
        throwErrnoIfMinus1Retry_ loc (c_stat s p)
        f p

isDirectory :: Ptr CStat -> IO Bool
isDirectory stat = do
  mode <- st_mode stat
  return (s_isdir mode)

fileNameEndClean :: String -> String
fileNameEndClean name = if isDrive name then addTrailingPathSeparator name
                                        else dropTrailingPathSeparator name
#endif

{- | Returns the current user's home directory.

The directory returned is expected to be writable by the current user,
but note that it isn't generally considered good practice to store
application-specific data here; use 'getXdgDirectory' or
'getAppUserDataDirectory' instead.

On Unix, 'getHomeDirectory' returns the value of the @HOME@
environment variable.  On Windows, the system is queried for a
suitable path; a typical path might be @C:\/Users\//\<user\>/@.

The operation may fail with:

* 'UnsupportedOperation'
The operating system has no notion of home directory.

* 'isDoesNotExistError'
The home directory for the current user does not exist, or
cannot be found.
-}
getHomeDirectory :: IO FilePath
getHomeDirectory = modifyIOError (`ioeSetLocation` "getHomeDirectory") get
  where
#if defined(mingw32_HOST_OS)
    get = getFolderPath Win32.cSIDL_PROFILE `catchIOError` \ _ ->
          getFolderPath Win32.cSIDL_WINDOWS
    getFolderPath what = Win32.sHGetFolderPath nullPtr what nullPtr 0
#else
    get = getEnv "HOME"
#endif

-- | Special directories for storing user-specific application data,
--   configuration, and cache files, as specified by the
--   <http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html XDG Base Directory Specification>.
--
--   Note: On Windows, 'XdgData' and 'XdgConfig' map to the same directory.
--
--   @since 1.2.3.0
data XdgDirectory
  = XdgData
    -- ^ For data files (e.g. images).
    --   Defaults to @~\/.local\/share@ and can be
    --   overridden by the @XDG_DATA_HOME@ environment variable.
    --   On Windows, it is @%APPDATA%@
    --   (e.g. @C:\/Users\//\<user\>/\/AppData\/Roaming@).
    --   Can be considered as the user-specific equivalent of @\/usr\/share@.
  | XdgConfig
    -- ^ For configuration files.
    --   Defaults to @~\/.config@ and can be
    --   overridden by the @XDG_CONFIG_HOME@ environment variable.
    --   On Windows, it is @%APPDATA%@
    --   (e.g. @C:\/Users\//\<user\>/\/AppData\/Roaming@).
    --   Can be considered as the user-specific equivalent of @\/etc@.
  | XdgCache
    -- ^ For non-essential files (e.g. cache).
    --   Defaults to @~\/.cache@ and can be
    --   overridden by the @XDG_CACHE_HOME@ environment variable.
    --   On Windows, it is @%LOCALAPPDATA%@
    --   (e.g. @C:\/Users\//\<user\>/\/AppData\/Local@).
    --   Can be considered as the user-specific equivalent of @\/var\/cache@.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Obtain the paths to special directories for storing user-specific
--   application data, configuration, and cache files, conforming to the
--   <http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html XDG Base Directory Specification>.
--   Compared with 'getAppUserDataDirectory', this function provides a more
--   fine-grained hierarchy as well as greater flexibility for the user.
--
--   It also works on Windows, although in that case 'XdgData' and 'XdgConfig'
--   will map to the same directory.
--
--   The second argument is usually the name of the application.  Since it
--   will be integrated into the path, it must consist of valid path
--   characters.
--
--   Note: The directory may not actually exist, in which case you would need
--   to create it with file mode @700@ (i.e. only accessible by the owner).
--
--   @since 1.2.3.0
getXdgDirectory :: XdgDirectory         -- ^ which special directory
                -> FilePath             -- ^ a relative path that is appended
                                        --   to the path; if empty, the base
                                        --   path is returned
                -> IO FilePath
getXdgDirectory xdgDir suffix =
  modifyIOError (`ioeSetLocation` "getXdgDirectory") $
  normalise . (</> suffix) <$>
  case xdgDir of
    XdgData   -> get False "XDG_DATA_HOME"   ".local/share"
    XdgConfig -> get False "XDG_CONFIG_HOME" ".config"
    XdgCache  -> get True  "XDG_CACHE_HOME"  ".cache"
  where
#if defined(mingw32_HOST_OS)
    get isLocal _ _ = Win32.sHGetFolderPath nullPtr which nullPtr 0
      where which | isLocal   = win32_cSIDL_LOCAL_APPDATA
                  | otherwise = Win32.cSIDL_APPDATA
#else
    get _ name fallback = do
      env <- lookupEnv name
      case env of
        Nothing                     -> fallback'
        Just path | isRelative path -> fallback'
                  | otherwise       -> return path
      where fallback' = (</> fallback) <$> getHomeDirectory

-- | Return the value of an environment variable, or 'Nothing' if there is no
--   such value.  (Equivalent to "lookupEnv" from base-4.6.)
lookupEnv :: String -> IO (Maybe String)
lookupEnv name = do
  env <- tryIOErrorType isDoesNotExistError (getEnv name)
  case env of
    Left  _     -> return Nothing
    Right value -> return (Just value)

-- | Similar to 'try' but only catches a specify kind of 'IOError' as
--   specified by the predicate.
tryIOErrorType :: (IOError -> Bool) -> IO a -> IO (Either IOError a)
tryIOErrorType check action = do
  result <- tryIOError action
  case result of
    Left  err -> if check err then return (Left err) else ioError err
    Right val -> return (Right val)
#endif

-- | Obtain the path to a special directory for storing user-specific
--   application data (traditional Unix location).  Except for backward
--   compatibility reasons, newer applications may prefer the the
--   XDG-conformant location provided by 'getXdgDirectory', which offers a
--   more fine-grained hierarchy as well as greater flexibility for the user
--   (<https://github.com/haskell/directory/issues/6#issuecomment-96521020 migration guide>).
--
--   The argument is usually the name of the application.  Since it will be
--   integrated into the path, it must consist of valid path characters.
--
--   * On Unix-like systems, the path is @~\/./\<app\>/@.
--   * On Windows, the path is @%APPDATA%\//\<app\>/@
--     (e.g. @C:\/Users\//\<user\>/\/AppData\/Roaming\//\<app\>/@)
--
--   Note: the directory may not actually exist, in which case you would need
--   to create it.  It is expected that the parent directory exists and is
--   writable.
--
--   The operation may fail with:
--
--   * 'UnsupportedOperation'
--     The operating system has no notion of application-specific data
--     directory.
--
--   * 'isDoesNotExistError'
--     The home directory for the current user does not exist, or cannot be
--     found.
--
getAppUserDataDirectory :: FilePath     -- ^ a relative path that is appended
                                        --   to the path
                        -> IO FilePath
getAppUserDataDirectory appName = do
  modifyIOError (`ioeSetLocation` "getAppUserDataDirectory") $ do
#if defined(mingw32_HOST_OS)
    s <- Win32.sHGetFolderPath nullPtr Win32.cSIDL_APPDATA nullPtr 0
    return (s++'\\':appName)
#else
    path <- getEnv "HOME"
    return (path++'/':'.':appName)
#endif

{- | Returns the current user's document directory.

The directory returned is expected to be writable by the current user,
but note that it isn't generally considered good practice to store
application-specific data here; use 'getXdgDirectory' or
'getAppUserDataDirectory' instead.

On Unix, 'getUserDocumentsDirectory' returns the value of the @HOME@
environment variable.  On Windows, the system is queried for a
suitable path; a typical path might be @C:\/Users\//\<user\>/\/Documents@.

The operation may fail with:

* 'UnsupportedOperation'
The operating system has no notion of document directory.

* 'isDoesNotExistError'
The document directory for the current user does not exist, or
cannot be found.
-}
getUserDocumentsDirectory :: IO FilePath
getUserDocumentsDirectory = do
  modifyIOError (`ioeSetLocation` "getUserDocumentsDirectory") $ do
#if defined(mingw32_HOST_OS)
    Win32.sHGetFolderPath nullPtr Win32.cSIDL_PERSONAL nullPtr 0
#else
    getEnv "HOME"
#endif

{- | Returns the current directory for temporary files.

On Unix, 'getTemporaryDirectory' returns the value of the @TMPDIR@
environment variable or \"\/tmp\" if the variable isn\'t defined.
On Windows, the function checks for the existence of environment variables in
the following order and uses the first path found:

*
TMP environment variable.

*
TEMP environment variable.

*
USERPROFILE environment variable.

*
The Windows directory

The operation may fail with:

* 'UnsupportedOperation'
The operating system has no notion of temporary directory.

The function doesn\'t verify whether the path exists.
-}
getTemporaryDirectory :: IO FilePath
getTemporaryDirectory =
#if defined(mingw32_HOST_OS)
  Win32.getTemporaryDirectory
#else
  getEnv "TMPDIR" `catchIOError` \ err ->
  if isDoesNotExistError err then return "/tmp" else ioError err
#endif
