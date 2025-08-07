-----------------------------------------------------------------------------
-- |
-- Module      :  System.Directory.OsPath
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- System-independent interface to directory manipulation.
--
-- @since 1.3.8.0
--
-----------------------------------------------------------------------------

module System.Directory.OsPath
   (
    -- $intro

    -- * Actions on directories
      createDirectory
    , createDirectoryIfMissing
    , removeDirectory
    , removeDirectoryRecursive
    , removePathForcibly
    , renameDirectory
    , listDirectory
    , getDirectoryContents
    -- ** Current working directory
    , getCurrentDirectory
    , setCurrentDirectory
    , withCurrentDirectory

    -- * Pre-defined directories
    , getHomeDirectory
    , XdgDirectory(..)
    , getXdgDirectory
    , XdgDirectoryList(..)
    , getXdgDirectoryList
    , getAppUserDataDirectory
    , getUserDocumentsDirectory
    , getTemporaryDirectory

    -- * PATH
    , getExecSearchPath

    -- * Actions on files
    , removeFile
    , renameFile
    , renamePath
    , copyFile
    , copyFileWithMetadata
    , getFileSize
    , replaceFile

    , canonicalizePath
    , makeAbsolute
    , makeRelativeToCurrentDirectory

    -- * Existence tests
    , doesPathExist
    , doesFileExist
    , doesDirectoryExist

    , findExecutable
    , findExecutables
    , findExecutablesInDirectories
    , findFile
    , findFiles
    , findFileWith
    , findFilesWith
    , exeExtension

    -- * Symbolic links
    , createFileLink
    , createDirectoryLink
    , removeDirectoryLink
    , pathIsSymbolicLink
    , getSymbolicLinkTarget

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
import Prelude ()
import System.Directory.Internal
import System.Directory.Internal.Prelude
import qualified System.File.OsPath as OS
import System.OsPath
  ( (<.>)
  , (</>)
  , addTrailingPathSeparator
  , dropTrailingPathSeparator
  , hasTrailingPathSeparator
  , isAbsolute
  , joinPath
  , makeRelative
  , splitDirectories
  , splitSearchPath
  , takeDirectory
  , encodeWith
  )
import qualified Data.List.NonEmpty as NE
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GHC.IO.Encoding.UTF8 ( mkUTF8 )
import GHC.IO.Encoding.UTF16 ( mkUTF16le )
import GHC.IO.Encoding.Failure ( CodingFailureMode(..) )

{- $intro
A directory contains a series of entries, each of which is a named
reference to a file system object (file, directory etc.).  Some
entries may be hidden, inaccessible, or have some administrative
function (e.g. @.@ or @..@ under
<http://www.opengroup.org/onlinepubs/009695399 POSIX>), but in
this standard all such entries are considered to form part of the
directory contents. Entries in sub-directories are not, however,
considered to form part of the directory contents.

Each file system object is referenced by a /path/.  There is
normally at least one absolute path to each file system object.  In
some operating systems, it may also be possible to have paths which
are relative to the current directory.

Unless otherwise documented:

* 'IO' operations in this package may throw any 'IOError'.  No other types of
  exceptions shall be thrown.

* The list of possible 'IOErrorType's in the API documentation is not
  exhaustive.  The full list may vary by platform and/or evolve over time.

-}

-----------------------------------------------------------------------------
-- Permissions

{- $permissions

directory offers a limited (and quirky) interface for reading and setting file
and directory permissions; see 'getPermissions' and 'setPermissions' for a
discussion of their limitations.  Because permissions are very difficult to
implement portably across different platforms, users who wish to do more
sophisticated things with permissions are advised to use other,
platform-specific libraries instead.  For example, if you are only interested
in permissions on POSIX-like platforms,
<https://hackage.haskell.org/package/unix/docs/System-Posix-Files.html unix>
offers much more flexibility.

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

-- | Get the permissions of a file or directory.
--
-- On Windows, the 'writable' permission corresponds to the "read-only"
-- attribute.  The 'executable' permission is set if the file extension is of
-- an executable file type.  The 'readable' permission is always set.
--
-- On POSIX systems, this returns the result of @access@.
--
-- The operation may fail with:
--
-- * 'isPermissionError' if the user is not permitted to access the
--   permissions, or
--
-- * 'isDoesNotExistError' if the file or directory does not exist.
getPermissions :: OsPath -> IO Permissions
getPermissions path =
  (`ioeAddLocation` "getPermissions") `modifyIOError` do
    getAccessPermissions (emptyToCurDir path)

-- | Set the permissions of a file or directory.
--
-- On Windows, this is only capable of changing the 'writable' permission,
-- which corresponds to the "read-only" attribute.  Changing the other
-- permissions has no effect.
--
-- On POSIX systems, this sets the /owner/ permissions.
--
-- The operation may fail with:
--
-- * 'isPermissionError' if the user is not permitted to set the permissions,
--   or
--
-- * 'isDoesNotExistError' if the file or directory does not exist.
setPermissions :: OsPath -> Permissions -> IO ()
setPermissions path p =
  (`ioeAddLocation` "setPermissions") `modifyIOError` do
    setAccessPermissions (emptyToCurDir path) p

-- | Copy the permissions of one file to another.  This reproduces the
-- permissions more accurately than using 'getPermissions' followed by
-- 'setPermissions'.
--
-- On Windows, this copies only the read-only attribute.
--
-- On POSIX systems, this is equivalent to @stat@ followed by @chmod@.
copyPermissions :: OsPath -> OsPath -> IO ()
copyPermissions src dst =
  (`ioeAddLocation` "copyPermissions") `modifyIOError` do
    m <- getFileMetadata src
    copyPermissionsFromMetadata m dst

copyPermissionsFromMetadata :: Metadata -> OsPath -> IO ()
copyPermissionsFromMetadata m dst = do
  -- instead of setFileMode, setFilePermissions is used here
  -- this is to retain backward compatibility in copyPermissions
  setFilePermissions dst (modeFromMetadata m)

-----------------------------------------------------------------------------
-- Implementation

{- |@'createDirectory' dir@ creates a new directory @dir@ which is
initially empty, or as near to empty as the operating system
allows.

The operation may fail with:

* 'isPermissionError'
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES]@

* 'isAlreadyExistsError'
The operand refers to a directory that already exists.
@ [EEXIST]@

* @HardwareFault@
A physical I\/O error has occurred.
@[EIO]@

* @InvalidArgument@
The operand is not a valid directory name.
@[ENAMETOOLONG, ELOOP]@

* 'isDoesNotExistError'
There is no path to the directory.
@[ENOENT, ENOTDIR]@

* 'System.IO.isFullError'
Insufficient resources (virtual memory, process file descriptors,
physical disk space, etc.) are available to perform the operation.
@[EDQUOT, ENOSPC, ENOMEM, EMLINK]@

* @InappropriateType@
The path refers to an existing non-directory object.
@[EEXIST]@

-}

createDirectory :: OsPath -> IO ()
createDirectory = createDirectoryInternal

-- | @'createDirectoryIfMissing' parents dir@ creates a new directory
-- @dir@ if it doesn\'t exist. If the first argument is 'True'
-- the function will also create all parent directories if they are missing.
createDirectoryIfMissing :: Bool     -- ^ Create its parents too?
                         -> OsPath -- ^ The path to the directory you want to make
                         -> IO ()
createDirectoryIfMissing create_parents path0
  | create_parents = createDirs (parents path0)
  | otherwise      = createDirs (take 1 (parents path0))
  where
    parents = reverse . scanl1 (</>) . splitDirectories . simplify

    createDirs []         = pure ()
    createDirs (dir:[])   = createDir dir ioError
    createDirs (dir:dirs) =
      createDir dir $ \_ -> do
        createDirs dirs
        createDir dir ioError

    createDir dir notExistHandler = do
      r <- tryIOError (createDirectory dir)
      case r of
        Right ()                   -> pure ()
        Left  e
          | isDoesNotExistError  e -> notExistHandler e
          -- createDirectory (and indeed POSIX mkdir) does not distinguish
          -- between a dir already existing and a file already existing. So we
          -- check for it here. Unfortunately there is a slight race condition
          -- here, but we think it is benign. It could report an exception in
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
              canIgnore <- pathIsDirectory dir
                             `catchIOError` \ _ ->
                               pure (isAlreadyExistsError e)
              unless canIgnore (ioError e)
          | otherwise              -> ioError e


{- | @'removeDirectory' dir@ removes an existing directory /dir/.  The
implementation may specify additional constraints which must be
satisfied before a directory can be removed (e.g. the directory has to
be empty, or may not be in use by other processes).  It is not legal
for an implementation to partially remove a directory unless the
entire directory is removed. A conformant implementation need not
support directory removal in all situations (e.g. removal of the root
directory).

The operation may fail with:

* @HardwareFault@
A physical I\/O error has occurred.
@[EIO]@

* @InvalidArgument@
The operand is not a valid directory name.
@[ENAMETOOLONG, ELOOP]@

* 'isDoesNotExistError'
The directory does not exist.
@[ENOENT, ENOTDIR]@

* 'isPermissionError'
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES, EPERM]@

* @UnsatisfiedConstraints@
Implementation-dependent constraints are not satisfied.
@[EBUSY, ENOTEMPTY, EEXIST]@

* @UnsupportedOperation@
The implementation does not support removal in this situation.
@[EINVAL]@

* @InappropriateType@
The operand refers to an existing non-directory object.
@[ENOTDIR]@

-}

removeDirectory :: OsPath -> IO ()
removeDirectory = removePathInternal True

type Preremover = Maybe RawHandle -> OsPath -> Metadata -> IO ()

noPreremover :: Preremover
noPreremover _ _ _ = pure ()

forcePreremover :: Preremover
forcePreremover dir path metadata = do
  when (fileTypeIsDirectory (fileTypeFromMetadata metadata)
        || not filesAlwaysRemovable) $ do
    setModeAt NoFollow dir path mode
      `catchIOError` \ _ -> pure ()
  where
    mode = setForceRemoveMode (modeFromMetadata metadata)

removeRecursivelyAt
  :: (IO () -> IO ())
  -> ([IO ()] -> IO ())
  -> Preremover
  -> Maybe RawHandle
  -> OsPath
  -> IO ()
removeRecursivelyAt catcher sequencer preremover dir name = catcher $ do
  metadata <- getMetadataAt NoFollow dir name
  preremover dir name metadata
  let
    fileType = fileTypeFromMetadata metadata
    subremovals = do
      when (fileType == Directory) $ do
        bracket (openRaw NoFollow dir name) closeRaw $ \ handle -> do
          -- dropSpecialDotDirs is extremely important! Otherwise it will
          -- recurse into the parent directory and wreak havoc.
          names <- dropSpecialDotDirs <$> readDirToEnd handle
          sequencer (recurse (Just handle) <$> names)
  sequencer [subremovals, removePathAt fileType dir name]
  where recurse = removeRecursivelyAt catcher sequencer preremover

-- | @'removeDirectoryRecursive' dir@ removes an existing directory /dir/
-- together with its contents and subdirectories. Within this directory,
-- symbolic links are removed without affecting their targets.
--
-- On Windows, the operation fails if /dir/ is a directory symbolic link.
--
-- This operation is reported to be flaky on Windows so retry logic may
-- be advisable. See: https://github.com/haskell/directory/pull/108
removeDirectoryRecursive :: OsPath -> IO ()
removeDirectoryRecursive path =
  (`ioeAddLocation` "removeDirectoryRecursive") `modifyIOError` do
    m <- getSymbolicLinkMetadata path
    case fileTypeFromMetadata m of
      Directory ->
        removeRecursivelyAt id sequenceA_ noPreremover Nothing path
      DirectoryLink ->
        ioError (err `ioeSetErrorString` "is a directory symbolic link")
      _ ->
        ioError (err `ioeSetErrorString` "not a directory")
  where err = mkIOError InappropriateType "" Nothing Nothing `ioeSetOsPath` path

-- | Removes a file or directory at /path/ together with its contents and
-- subdirectories. Symbolic links are removed without affecting their
-- targets. If the path does not exist, nothing happens.
--
-- Unlike other removal functions, this function will also attempt to delete
-- files marked as read-only or otherwise made unremovable due to permissions.
-- As a result, if the removal is incomplete, the permissions or attributes on
-- the remaining files may be altered.  If there are hard links in the
-- directory, then permissions on all related hard links may be altered.
--
-- If an entry within the directory vanishes while @removePathForcibly@ is
-- running, it is silently ignored.
--
-- If an exception occurs while removing an entry, @removePathForcibly@ will
-- still try to remove as many entries as it can before failing with an
-- exception.  The first exception that it encountered is re-thrown.
removePathForcibly :: OsPath -> IO ()
removePathForcibly path =
  (`ioeAddLocation` "removePathForcibly") `modifyIOError` do
    removeRecursivelyAt
      ignoreDoesNotExistError
      sequenceWithIOErrors_
      forcePreremover
      Nothing
      path

  where

    ignoreDoesNotExistError :: IO () -> IO ()
    ignoreDoesNotExistError action =
      () <$ tryIOErrorType isDoesNotExistError action

{- |'removeFile' /file/ removes the directory entry for an existing file
/file/, where /file/ is not itself a directory. The
implementation may specify additional constraints which must be
satisfied before a file can be removed (e.g. the file may not be in
use by other processes).

The operation may fail with:

* @HardwareFault@
A physical I\/O error has occurred.
@[EIO]@

* @InvalidArgument@
The operand is not a valid file name.
@[ENAMETOOLONG, ELOOP]@

* 'isDoesNotExistError'
The file does not exist.
@[ENOENT, ENOTDIR]@

* 'isPermissionError'
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES, EPERM]@

* @UnsatisfiedConstraints@
Implementation-dependent constraints are not satisfied.
@[EBUSY]@

* @InappropriateType@
The operand refers to an existing directory.
@[EPERM, EINVAL]@

-}

removeFile :: OsPath -> IO ()
removeFile = removePathInternal False

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

* @HardwareFault@
A physical I\/O error has occurred.
@[EIO]@

* @InvalidArgument@
Either operand is not a valid directory name.
@[ENAMETOOLONG, ELOOP]@

* 'isDoesNotExistError'
The original directory does not exist, or there is no path to the target.
@[ENOENT, ENOTDIR]@

* 'isPermissionError'
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES, EPERM]@

* 'System.IO.isFullError'
Insufficient resources are available to perform the operation.
@[EDQUOT, ENOSPC, ENOMEM, EMLINK]@

* @UnsatisfiedConstraints@
Implementation-dependent constraints are not satisfied.
@[EBUSY, ENOTEMPTY, EEXIST]@

* @UnsupportedOperation@
The implementation does not support renaming in this situation.
@[EINVAL, EXDEV]@

* @InappropriateType@
Either path refers to an existing non-directory object.
@[ENOTDIR, EISDIR]@

-}

renameDirectory :: OsPath -> OsPath -> IO ()
renameDirectory opath npath =
   (`ioeAddLocation` "renameDirectory") `modifyIOError` do
     -- XXX this test isn't performed atomically with the following rename
     isDir <- pathIsDirectory opath
     when (not isDir) . ioError $
       mkIOError InappropriateType "renameDirectory" Nothing Nothing
       `ioeSetErrorString` "not a directory"
       `ioeSetOsPath` opath
     renamePath opath npath

{- |@'renameFile' old new@ changes the name of an existing file system
object from /old/ to /new/.  If the /new/ object already exists, it is
replaced by the /old/ object.  Neither path may refer to an existing
directory.

A conformant implementation need not support renaming files in all situations
(e.g. renaming across different physical devices), but the constraints must be
documented. On Windows, this does not support renaming across different physical
devices; if you are looking to do so, consider using 'copyFileWithMetadata' and
'removeFile'.

On Windows, this calls @MoveFileEx@ with @MOVEFILE_REPLACE_EXISTING@ set,
which is not guaranteed to be atomic
(<https://github.com/haskell/directory/issues/109>).

On other platforms, this operation is atomic.

The operation may fail with:

* @HardwareFault@
A physical I\/O error has occurred.
@[EIO]@

* @InvalidArgument@
Either operand is not a valid file name.
@[ENAMETOOLONG, ELOOP]@

* 'isDoesNotExistError'
The original file does not exist, or there is no path to the target.
@[ENOENT, ENOTDIR]@

* 'isPermissionError'
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES, EPERM]@

* 'System.IO.isFullError'
Insufficient resources are available to perform the operation.
@[EDQUOT, ENOSPC, ENOMEM, EMLINK]@

* @UnsatisfiedConstraints@
Implementation-dependent constraints are not satisfied.
@[EBUSY]@

* @UnsupportedOperation@
The implementation does not support renaming in this situation.
@[EXDEV]@

* @InappropriateType@
Either path refers to an existing directory.
@[ENOTDIR, EISDIR, EINVAL, EEXIST, ENOTEMPTY]@

-}

renameFile :: OsPath -> OsPath -> IO ()
renameFile opath npath =
  (`ioeAddLocation` "renameFile") `modifyIOError` do
    -- XXX the tests are not performed atomically with the rename
    checkNotDir opath
    renamePath opath npath
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
          m <- tryIOError (getSymbolicLinkMetadata path)
          case fileTypeIsDirectory . fileTypeFromMetadata <$> m of
            Right True ->
              ioError $
              mkIOError InappropriateType "" Nothing Nothing
              `ioeSetErrorString` "is a directory"
              `ioeSetOsPath` path
            _          -> pure ()

-- | Rename a file or directory.  If the destination path already exists, it
-- is replaced atomically on unix.  If the destination path already exists and
-- destination on the same volume, it is replaced atomically on Windows.
-- The destination path must not point to an existing directory.  A conformant
-- implementation need not support renaming files in all situations
-- (e.g. renaming across different physical devices), but the constraints must
-- be documented.
--
-- The operation on unix may fail with:
--
-- * @HardwareFault@
-- A physical I\/O error has occurred.
-- @[EIO]@
--
-- * @InvalidArgument@
-- Either operand is not a valid file name.
-- @[ENAMETOOLONG, ELOOP]@
--
-- * 'isDoesNotExistError'
-- The original file does not exist, or there is no path to the target.
-- @[ENOENT, ENOTDIR]@
--
-- * 'isPermissionError'
-- The process has insufficient privileges to perform the operation.
-- @[EROFS, EACCES, EPERM]@
--
-- * 'System.IO.isFullError'
-- Insufficient resources are available to perform the operation.
-- @[EDQUOT, ENOSPC, ENOMEM, EMLINK]@
--
-- * @UnsatisfiedConstraints@
-- Implementation-dependent constraints are not satisfied.
-- @[EBUSY]@
--
-- * @UnsupportedOperation@
-- The implementation does not support renaming in this situation.
-- @[EXDEV]@
--
-- * @InappropriateType@
-- Either the destination path refers to an existing directory, or one of the
-- parent segments in the destination path is not a directory.
-- @[ENOTDIR, EISDIR, EINVAL, EEXIST, ENOTEMPTY]@
--
-- The operation on Windows may fail with:
--
-- ERROR_FILE_NOT_FOUND 2 (0x2)
-- The system cannot find the specified file.
--
-- ERROR_PATH_NOT_FOUND 3 (0x3)
-- The system cannot find the specified path.
--
-- ERROR_ACCESS_DENIED 5 (0x5)
-- Access to the file or resource is denied.
--
-- ERROR_ALREADY_EXISTS 183 (0xB7)
-- The file already exists and cannot be overwritten or recreated.
--
-- ERROR_SHARING_VIOLATION 32 (0x20)
-- The file is in use by another process and cannot be accessed.
--
-- ERROR_NOT_SAME_DEVICE 17 (0x11)
-- The operation cannot be performed across different storage devices.
--
-- ERROR_INVALID_PARAMETER 87 (0x57)
-- An invalid parameter was passed to the function.
--
-- ERROR_WRITE_PROTECT 19 (0x13)
-- The storage media is write-protected and cannot be modified.
--
-- ERROR_LOCK_VIOLATION 33 (0x21)
-- The file is locked by another process and cannot be accessed.
renamePath :: OsPath                  -- ^ Old path
           -> OsPath                  -- ^ New path
           -> IO ()
renamePath opath npath =
  (`ioeAddLocation` "renamePath") `modifyIOError` do
    renamePathInternal opath npath

-- | Replaces one file with another file. The replacement file assumes the name
-- of the replaced file and its identity.
-- 
-- Note on Windows atomicity:
-- File replacement is typically atomic when both files are on the same volume and
-- no special file system features interfere. If the files are on different volumes,
-- or if a system crash or power failure occurs during the operation, atomicity is
-- not guaranteed and the destination file may be left in an inconsistent state.
--
-- On the unix same as renamePath, on the Windows platform this is ReplaceFileW.
--
-- The operation on unix may fail with:
--
-- * @HardwareFault@
-- A physical I\/O error has occurred.
-- @[EIO]@
--
-- * @InvalidArgument@
-- Either operand is not a valid file name.
-- @[ENAMETOOLONG, ELOOP]@
--
-- * 'isDoesNotExistError'
-- The original file does not exist, or there is no path to the target.
-- @[ENOENT, ENOTDIR]@
--
-- * 'isPermissionError'
-- The process has insufficient privileges to perform the operation.
-- @[EROFS, EACCES, EPERM]@
--
-- * 'System.IO.isFullError'
-- Insufficient resources are available to perform the operation.
-- @[EDQUOT, ENOSPC, ENOMEM, EMLINK]@
--
-- * @UnsatisfiedConstraints@
-- Implementation-dependent constraints are not satisfied.
-- @[EBUSY]@
--
-- * @UnsupportedOperation@
-- The implementation does not support renaming in this situation.
-- @[EXDEV]@
--
-- * @InappropriateType@
-- Either the destination path refers to an existing directory, or one of the
-- parent segments in the destination path is not a directory.
-- @[ENOTDIR, EISDIR, EINVAL, EEXIST, ENOTEMPTY]@
--
-- The operation on Windows may fail with:
--
-- ERROR_FILE_NOT_FOUND 2 (0x2)
-- The system cannot find the specified file.
--
-- ERROR_PATH_NOT_FOUND 3 (0x3)
-- The system cannot find the specified path.
--
-- ERROR_ACCESS_DENIED 5 (0x5)
-- Access to the file or resource is denied.
--
-- ERROR_SHARING_VIOLATION 32 (0x20)
-- The file is in use by another process and cannot be accessed.
--
-- ERROR_INVALID_PARAMETER 87 (0x57)
-- An invalid parameter was passed to the function.
--
-- ERROR_UNABLE_TO_REMOVE_REPLACED 1175 (0x497)
-- The replaced file could not be deleted. The replaced and replacement files
-- retain their original file names.
--
-- ERROR_UNABLE_TO_MOVE_REPLACEMENT 1176 (0x498)
-- The replacement file could not be renamed. The replaced file no longer exists
-- and the replacement file remains under its original name.
--
-- ERROR_UNABLE_TO_MOVE_REPLACEMENT_2 1177 (0x499)
-- The replacement file could not be moved. It still exists under its original name
-- but has inherited attributes from the target file. The original target file
-- persists under a different name.
replaceFile :: OsPath                 -- ^ File to be replaced
            -> OsPath                 -- ^ Replacement file
            -> IO ()
replaceFile opath npath =
  (`ioeAddLocation` "replaceFile") `modifyIOError` do
    replaceFileInternal opath npath Nothing

-- | Copy a file with its permissions.  If the destination file already exists,
-- it is replaced atomically.  Neither path may refer to an existing
-- directory.  No exceptions are thrown if the permissions could not be
-- copied.
copyFile :: OsPath                    -- ^ Source filename
         -> OsPath                    -- ^ Destination filename
         -> IO ()
copyFile fromFPath toFPath =
  (`ioeAddLocation` "copyFile") `modifyIOError` do
    atomicCopyFileContents fromFPath toFPath
      (ignoreIOExceptions . copyPermissions fromFPath)

-- | Copy all data from a file to a handle.
copyFileToHandle :: OsPath              -- ^ Source file
                 -> Handle              -- ^ Destination handle
                 -> IO ()
copyFileToHandle fromFPath hTo =
  (`ioeAddLocation` "copyFileToHandle") `modifyIOError` do
    withBinaryFile fromFPath ReadMode $ \ hFrom ->
      copyHandleData hFrom hTo

-- | Copy the contents of a source file to a destination file, replacing the
-- destination file atomically via @withReplacementFile@, resetting the
-- attributes of the destination file to the defaults.
atomicCopyFileContents :: OsPath            -- ^ Source filename
                       -> OsPath            -- ^ Destination filename
                       -> (OsPath -> IO ()) -- ^ Post-action
                       -> IO ()
atomicCopyFileContents fromFPath toFPath postAction =
  (`ioeAddLocation` "atomicCopyFileContents") `modifyIOError` do
    withReplacementFile toFPath postAction $ \ hTo -> do
      copyFileToHandle fromFPath hTo

-- | A helper function useful for replacing files in an atomic manner.  The
-- function creates a temporary file in the directory of the destination file,
-- opens it, performs the main action with its handle, closes it, performs the
-- post-action with its path, and finally replaces the destination file with
-- the temporary file.  If an error occurs during any step of this process,
-- the temporary file is removed and the destination file remains untouched.
withReplacementFile :: OsPath            -- ^ Destination file
                    -> (OsPath -> IO ()) -- ^ Post-action
                    -> (Handle -> IO a)    -- ^ Main action
                    -> IO a
withReplacementFile path postAction action =
  (`ioeAddLocation` "withReplacementFile") `modifyIOError` do
    mask $ \ restore -> do
      let tmpPath = case encodeWith (mkUTF8 ErrorOnCodingFailure)
                                    (mkUTF16le ErrorOnCodingFailure)
                                    ".copyFile.tmp"
                         of
            Left err -> error ("withReplacementFile: invalid encoding: " ++ show err)
            Right p -> p
      (tmpFPath, hTmp) <- OS.openBinaryTempFile (takeDirectory path) tmpPath
      (`onException` ignoreIOExceptions (removeFile tmpFPath)) $ do
        r <- (`onException` ignoreIOExceptions (hClose hTmp)) $ do
          restore (action hTmp)
        hClose hTmp
        restore (postAction tmpFPath)
        renameFile tmpFPath path
        pure r

-- | Copy a file with its associated metadata.  If the destination file
-- already exists, it is overwritten.  There is no guarantee of atomicity in
-- the replacement of the destination file.  Neither path may refer to an
-- existing directory.  If the source and/or destination are symbolic links,
-- the copy is performed on the targets of the links.
--
-- On Windows, it behaves like the Win32 function
-- <https://msdn.microsoft.com/en-us/library/windows/desktop/aa363851.aspx CopyFile>,
-- which copies various kinds of metadata including file attributes and
-- security resource properties.
--
-- On Unix-like systems, permissions, access time, and modification time are
-- preserved.  If possible, the owner and group are also preserved.  Note that
-- the very act of copying can change the access time of the source file,
-- hence the access times of the two files may differ after the operation
-- completes.
copyFileWithMetadata :: OsPath        -- ^ Source file
                     -> OsPath        -- ^ Destination file
                     -> IO ()
copyFileWithMetadata src dst =
  (`ioeAddLocation` "copyFileWithMetadata") `modifyIOError`
    copyFileWithMetadataInternal copyPermissionsFromMetadata
                                 copyTimesFromMetadata
                                 src
                                 dst

copyTimesFromMetadata :: Metadata -> OsPath -> IO ()
copyTimesFromMetadata st dst = do
  let atime = accessTimeFromMetadata st
  let mtime = modificationTimeFromMetadata st
  setFileTimes dst (Just atime, Just mtime)

-- | Make a path absolute, normalize the path, and remove as many indirections
-- from it as possible.  Any trailing path separators are discarded via
-- 'dropTrailingPathSeparator'.  Additionally, on Windows the letter case of
-- the path is canonicalized.
--
-- __Note__: This function is a very big hammer.  If you only need an absolute
-- path, 'makeAbsolute' is sufficient for removing dependence on the current
-- working directory.
--
-- Indirections include the two special directories @.@ and @..@, as well as
-- any symbolic links (and junction points on Windows).  The input path need
-- not point to an existing file or directory.  Canonicalization is performed
-- on the longest prefix of the path that points to an existing file or
-- directory.  The remaining portion of the path that does not point to an
-- existing file or directory will still be normalized, but case
-- canonicalization and indirection removal are skipped as they are impossible
-- to do on a nonexistent path.
--
-- Most programs should not worry about the canonicity of a path.  In
-- particular, despite the name, the function does not truly guarantee
-- canonicity of the returned path due to the presence of hard links, mount
-- points, etc.
--
-- If the path points to an existing file or directory, then the output path
-- shall also point to the same file or directory, subject to the condition
-- that the relevant parts of the file system do not change while the function
-- is still running.  In other words, the function is definitively not atomic.
-- The results can be utterly wrong if the portions of the path change while
-- this function is running.
--
-- Since some indirections (symbolic links on all systems, @..@ on non-Windows
-- systems, and junction points on Windows) are dependent on the state of the
-- existing filesystem, the function can only make a conservative attempt by
-- removing such indirections from the longest prefix of the path that still
-- points to an existing file or directory.
--
-- Note that on Windows parent directories @..@ are always fully expanded
-- before the symbolic links, as consistent with the rest of the Windows API
-- (such as @GetFullPathName@).  In contrast, on POSIX systems parent
-- directories @..@ are expanded alongside symbolic links from left to right.
-- To put this more concretely: if @L@ is a symbolic link for @R/P@, then on
-- Windows @L\\..@ refers to @.@, whereas on other operating systems @L/..@
-- refers to @R@.
--
-- Similar to 'System.FilePath.normalise', passing an empty path is equivalent
-- to passing the current directory.
--
-- @canonicalizePath@ can resolve at least 64 indirections in a single path,
-- more than what is supported by most operating systems.  Therefore, it may
-- return the fully resolved path even though the operating system itself
-- would have long given up.
--
-- On Windows XP or earlier systems, junction expansion is not performed due
-- to their lack of @GetFinalPathNameByHandle@.
--
-- /Changes since 1.2.3.0:/ The function has been altered to be more robust
-- and has the same exception behavior as 'makeAbsolute'.
--
-- /Changes since 1.3.0.0:/ The function no longer preserves the trailing path
-- separator.  File symbolic links that appear in the middle of a path are
-- properly dereferenced.  Case canonicalization and symbolic link expansion
-- are now performed on Windows.
--
canonicalizePath :: OsPath -> IO OsPath
canonicalizePath = \ path ->
  ((`ioeAddLocation` "canonicalizePath") .
   (`ioeSetOsPath` path)) `modifyIOError` do
    -- simplify does more stuff, like upper-casing the drive letter
    dropTrailingPathSeparator . simplify <$>
      (attemptRealpath realPath =<< prependCurrentDirectory path)
  where

    -- allow up to 64 cycles before giving up
    attemptRealpath realpath =
      attemptRealpathWith (64 :: Int) Nothing realpath
      <=< canonicalizePathSimplify

    -- n is a counter to make sure we don't run into an infinite loop; we
    -- don't try to do any cycle detection here because an adversary could DoS
    -- any arbitrarily clever algorithm
    attemptRealpathWith n mFallback realpath path =
      case mFallback of
        -- too many indirections ... giving up.
        Just fallback | n <= 0 -> pure fallback
        -- either mFallback == Nothing (first attempt)
        --     or n > 0 (still have some attempts left)
        _ -> realpathPrefix (reverse (zip prefixes suffixes))

      where

        segments = splitDirectories path
        prefixes = scanl1 (</>) segments
        suffixes = NE.tail (NE.scanr (</>) mempty segments)

        -- try to call realpath on the largest possible prefix
        realpathPrefix candidates =
          case candidates of
            [] -> pure path
            (prefix, suffix) : rest -> do
              exist <- doesPathExist prefix
              if not exist
                -- never call realpath on an inaccessible path
                -- (to avoid bugs in system realpath implementations)
                -- try a smaller prefix instead
                then realpathPrefix rest
                else do
                  mp <- tryIOError (realpath prefix)
                  case mp of
                    -- realpath failed: try a smaller prefix instead
                    Left _ -> realpathPrefix rest
                    -- realpath succeeded: fine-tune the result
                    Right p -> realpathFurther (p </> suffix) p suffix

        -- by now we have a reasonable fallback value that we can use if we
        -- run into too many indirections; the fallback value is the same
        -- result that we have been returning in versions prior to 1.3.1.0
        -- (this is essentially the fix to #64)
        realpathFurther fallback p suffix =
          case splitDirectories suffix of
            [] -> pure fallback
            next : restSuffix -> do
              -- see if the 'next' segment is a symlink
              mTarget <- tryIOError (getSymbolicLinkTarget (p </> next))
              case mTarget of
                Left _ -> pure fallback
                Right target -> do
                  -- if so, dereference it and restart the whole cycle
                  let mFallback' = Just (fromMaybe fallback mFallback)
                  path' <- canonicalizePathSimplify
                             (p </> target </> joinPath restSuffix)
                  attemptRealpathWith (n - 1) mFallback' realpath path'

-- | Convert a path into an absolute path.  If the given path is relative, the
-- current directory is prepended and then the combined result is normalized.
-- If the path is already absolute, the path is simply normalized.  The
-- function preserves the presence or absence of the trailing path separator
-- unless the path refers to the root directory @/@.
--
-- If the path is already absolute, the operation never fails.  Otherwise, the
-- operation may fail with the same exceptions as 'getCurrentDirectory'.
--
makeAbsolute :: OsPath -> IO OsPath
makeAbsolute path =
  ((`ioeAddLocation` "makeAbsolute") .
   (`ioeSetOsPath` path)) `modifyIOError` do
    matchTrailingSeparator path . simplify <$> prependCurrentDirectory path

-- | Add or remove the trailing path separator in the second path so as to
-- match its presence in the first path.
--
-- (internal API)
matchTrailingSeparator :: OsPath -> OsPath -> OsPath
matchTrailingSeparator path
  | hasTrailingPathSeparator path = addTrailingPathSeparator
  | otherwise                     = dropTrailingPathSeparator

-- | Construct a path relative to the current directory, similar to
-- 'makeRelative'.
--
-- The operation may fail with the same exceptions as 'getCurrentDirectory'.
makeRelativeToCurrentDirectory :: OsPath -> IO OsPath
makeRelativeToCurrentDirectory x = do
  (`makeRelative` x) <$> getCurrentDirectory

-- | Given the name or path of an executable file, 'findExecutable' searches
-- for such a file in a list of system-defined locations, which generally
-- includes @PATH@ and possibly more.  The full path to the executable is
-- returned if found.  For example, @(findExecutable \"ghc\")@ would normally
-- give you the path to GHC.
--
-- The path returned by @'findExecutable' name@ corresponds to the program
-- that would be executed by
-- @<http://hackage.haskell.org/package/process/docs/System-Process.html#v:createProcess createProcess>@
-- when passed the same string (as a @RawCommand@, not a @ShellCommand@),
-- provided that @name@ is not a relative path with more than one segment.
--
-- On Windows, 'findExecutable' calls the Win32 function
-- @<https://msdn.microsoft.com/en-us/library/aa365527.aspx SearchPath>@,
-- which may search other places before checking the directories in the @PATH@
-- environment variable.  Where it actually searches depends on registry
-- settings, but notably includes the directory containing the current
-- executable.
--
-- On non-Windows platforms, the behavior is equivalent to 'findFileWith'
-- using the search directories from the @PATH@ environment variable and
-- testing each file for executable permissions.  Details can be found in the
-- documentation of 'findFileWith'.
findExecutable :: OsString -> IO (Maybe OsPath)
findExecutable binary =
  listTHead
    (findExecutablesLazyInternal findExecutablesInDirectoriesLazy binary)

-- | Search for executable files in a list of system-defined locations, which
-- generally includes @PATH@ and possibly more.
--
-- On Windows, this /only returns the first occurrence/, if any.  Its behavior
-- is therefore equivalent to 'findExecutable'.
--
-- On non-Windows platforms, the behavior is equivalent to
-- 'findExecutablesInDirectories' using the search directories from the @PATH@
-- environment variable.  Details can be found in the documentation of
-- 'findExecutablesInDirectories'.
findExecutables :: OsString -> IO [OsPath]
findExecutables binary =
  listTToList
    (findExecutablesLazyInternal findExecutablesInDirectoriesLazy binary)

-- | Given a name or path, 'findExecutable' appends the 'exeExtension' to the
-- query and searches for executable files in the list of given search
-- directories and returns all occurrences.
--
-- The behavior is equivalent to 'findFileWith' using the given search
-- directories and testing each file for executable permissions.  Details can
-- be found in the documentation of 'findFileWith'.
--
-- Unlike other similarly named functions, 'findExecutablesInDirectories' does
-- not use @SearchPath@ from the Win32 API.  The behavior of this function on
-- Windows is therefore equivalent to those on non-Windows platforms.
findExecutablesInDirectories :: [OsPath] -> OsString -> IO [OsPath]
findExecutablesInDirectories path binary =
  listTToList (findExecutablesInDirectoriesLazy path binary)

findExecutablesInDirectoriesLazy :: [OsPath] -> OsString -> ListT IO OsPath
findExecutablesInDirectoriesLazy path binary =
  findFilesWithLazy isExecutable path (binary <.> exeExtension)

-- | Test whether a file has executable permissions.
isExecutable :: OsPath -> IO Bool
isExecutable file = executable <$> getPermissions file

-- | Search through the given list of directories for the given file.
--
-- The behavior is equivalent to 'findFileWith', returning only the first
-- occurrence.  Details can be found in the documentation of 'findFileWith'.
findFile :: [OsPath] -> OsString -> IO (Maybe OsPath)
findFile = findFileWith (\ _ -> pure True)

-- | Search through the given list of directories for the given file and
-- returns all paths where the given file exists.
--
-- The behavior is equivalent to 'findFilesWith'.  Details can be found in the
-- documentation of 'findFilesWith'.
findFiles :: [OsPath] -> OsString -> IO [OsPath]
findFiles = findFilesWith (\ _ -> pure True)

-- | Search through a given list of directories for a file that has the given
-- name and satisfies the given predicate and return the path of the first
-- occurrence.  The directories are checked in a left-to-right order.
--
-- This is essentially a more performant version of 'findFilesWith' that
-- always returns the first result, if any.  Details can be found in the
-- documentation of 'findFilesWith'.
findFileWith :: (OsPath -> IO Bool) -> [OsPath] -> OsString -> IO (Maybe OsPath)
findFileWith f ds name = listTHead (findFilesWithLazy f ds name)

-- | @findFilesWith predicate dirs name@ searches through the list of
-- directories (@dirs@) for files that have the given @name@ and satisfy the
-- given @predicate@ and returns the paths of those files.  The directories
-- are checked in a left-to-right order and the paths are returned in the same
-- order.
--
-- If the @name@ is a relative path, then for every search directory @dir@,
-- the function checks whether @dir '</>' name@ exists and satisfies the
-- predicate.  If so, @dir '</>' name@ is returned as one of the results.  In
-- other words, the returned paths can be either relative or absolute
-- depending on the search directories were used.  If there are no search
-- directories, no results are ever returned.
--
-- If the @name@ is an absolute path, then the function will return a single
-- result if the file exists and satisfies the predicate and no results
-- otherwise.  This is irrespective of what search directories were given.
findFilesWith :: (OsPath -> IO Bool) -> [OsPath] -> OsString -> IO [OsPath]
findFilesWith f ds name = listTToList (findFilesWithLazy f ds name)

findFilesWithLazy
  :: (OsPath -> IO Bool) -> [OsPath] -> OsString -> ListT IO OsPath
findFilesWithLazy f dirs path
  -- make sure absolute paths are handled properly irrespective of 'dirs'
  -- https://github.com/haskell/directory/issues/72
  | isAbsolute path = ListT (find [mempty])
  | otherwise       = ListT (find dirs)

  where

    find []       = pure Nothing
    find (d : ds) = do
      let p = d </> path
      found <- doesFileExist p `andM` f p
      if found
        then pure (Just (p, ListT (find ds)))
        else find ds

-- | Filename extension for executable files (including the dot if any)
--   (usually @\"\"@ on POSIX systems and @\".exe\"@ on Windows or OS\/2).
exeExtension :: OsString
exeExtension = exeExtensionInternal

-- | Similar to 'listDirectory', but always includes the special entries (@.@
-- and @..@).  (This applies to Windows as well.)
--
-- The operation may fail with the same exceptions as 'listDirectory'.
getDirectoryContents :: OsPath -> IO [OsPath]
getDirectoryContents path =
  ((`ioeSetOsPath` path) .
   (`ioeAddLocation` "getDirectoryContents")) `modifyIOError` do
    getDirectoryContentsInternal path

-- | @'listDirectory' dir@ returns a list of /all/ entries in /dir/ without
-- the special entries (@.@ and @..@).
--
-- The operation may fail with:
--
-- * @HardwareFault@
--   A physical I\/O error has occurred.
--   @[EIO]@
--
-- * @InvalidArgument@
--   The operand is not a valid directory name.
--   @[ENAMETOOLONG, ELOOP]@
--
-- * 'isDoesNotExistError'
--   The directory does not exist.
--   @[ENOENT, ENOTDIR]@
--
-- * 'isPermissionError'
--   The process has insufficient privileges to perform the operation.
--   @[EACCES]@
--
-- * 'System.IO.isFullError'
--   Insufficient resources are available to perform the operation.
--   @[EMFILE, ENFILE]@
--
-- * @InappropriateType@
--   The path refers to an existing non-directory object.
--   @[ENOTDIR]@
--
listDirectory :: OsPath -> IO [OsPath]
listDirectory path = dropSpecialDotDirs <$> getDirectoryContents path

-- | Obtain the current working directory as an absolute path.
--
-- In a multithreaded program, the current working directory is a global state
-- shared among all threads of the process.  Therefore, when performing
-- filesystem operations from multiple threads, it is highly recommended to
-- use absolute rather than relative paths (see: 'makeAbsolute').
--
-- Note that 'getCurrentDirectory' is not guaranteed to return the same path
-- received by 'setCurrentDirectory'. On POSIX systems, the path returned will
-- always be fully dereferenced (not contain any symbolic links). For more
-- information, refer to the documentation of
-- <https://pubs.opengroup.org/onlinepubs/9699919799/functions/getcwd.html getcwd>.
--
-- The operation may fail with:
--
-- * @HardwareFault@
-- A physical I\/O error has occurred.
-- @[EIO]@
--
-- * 'isDoesNotExistError'
-- There is no path referring to the working directory.
-- @[EPERM, ENOENT, ESTALE...]@
--
-- * 'isPermissionError'
-- The process has insufficient privileges to perform the operation.
-- @[EACCES]@
--
-- * 'System.IO.isFullError'
-- Insufficient resources are available to perform the operation.
--
-- * @UnsupportedOperation@
-- The operating system has no notion of current working directory.
--
getCurrentDirectory :: IO OsPath
getCurrentDirectory =
  (`ioeAddLocation` "getCurrentDirectory") `modifyIOError` do
    specializeErrorString
      "Current working directory no longer exists"
      isDoesNotExistError
      getCurrentDirectoryInternal

-- | Change the working directory to the given path.
--
-- In a multithreaded program, the current working directory is a global state
-- shared among all threads of the process.  Therefore, when performing
-- filesystem operations from multiple threads, it is highly recommended to
-- use absolute rather than relative paths (see: 'makeAbsolute').
--
-- The operation may fail with:
--
-- * @HardwareFault@
-- A physical I\/O error has occurred.
-- @[EIO]@
--
-- * @InvalidArgument@
-- The operand is not a valid directory name.
-- @[ENAMETOOLONG, ELOOP]@
--
-- * 'isDoesNotExistError'
-- The directory does not exist.
-- @[ENOENT, ENOTDIR]@
--
-- * 'isPermissionError'
-- The process has insufficient privileges to perform the operation.
-- @[EACCES]@
--
-- * @UnsupportedOperation@
-- The operating system has no notion of current working directory, or the
-- working directory cannot be dynamically changed.
--
-- * @InappropriateType@
-- The path refers to an existing non-directory object.
-- @[ENOTDIR]@
--
setCurrentDirectory :: OsPath -> IO ()
setCurrentDirectory = setCurrentDirectoryInternal

-- | Run an 'IO' action with the given working directory and restore the
-- original working directory afterwards, even if the given action fails due
-- to an exception.
--
-- The operation may fail with the same exceptions as 'getCurrentDirectory'
-- and 'setCurrentDirectory'.
--
withCurrentDirectory :: OsPath    -- ^ Directory to execute in
                     -> IO a      -- ^ Action to be executed
                     -> IO a
withCurrentDirectory dir action =
  bracket getCurrentDirectory setCurrentDirectory $ \ _ -> do
    setCurrentDirectory dir
    action

-- | Obtain the size of a file in bytes.
getFileSize :: OsPath -> IO Integer
getFileSize path =
  (`ioeAddLocation` "getFileSize") `modifyIOError` do
    fileSizeFromMetadata <$> getFileMetadata path

-- | Test whether the given path points to an existing filesystem object.  If
-- the user lacks necessary permissions to search the parent directories, this
-- function may return false even if the file does actually exist.  This
-- operation traverses symbolic links, so it can return either True or False
-- for them.
doesPathExist :: OsPath -> IO Bool
doesPathExist path = do
  (True <$ getFileMetadata path)
    `catchIOError` \ _ ->
      pure False

-- | The operation 'doesDirectoryExist' returns 'True' if the argument file
-- exists and is either a directory or a symbolic link to a directory, and
-- 'False' otherwise.  This operation traverses symbolic links, so it can
-- return either True or False for them.
doesDirectoryExist :: OsPath -> IO Bool
doesDirectoryExist path = do
  pathIsDirectory path
    `catchIOError` \ _ ->
      pure False

-- | The operation 'doesFileExist' returns 'True' if the argument file exists
-- and is not a directory, and 'False' otherwise.  This operation traverses
-- symbolic links, so it can return either True or False for them.
doesFileExist :: OsPath -> IO Bool
doesFileExist path = do
  (not <$> pathIsDirectory path)
    `catchIOError` \ _ ->
      pure False

pathIsDirectory :: OsPath -> IO Bool
pathIsDirectory path =
  (`ioeAddLocation` "pathIsDirectory") `modifyIOError` do
    fileTypeIsDirectory . fileTypeFromMetadata <$> getFileMetadata path

-- | Create a /file/ symbolic link.  The target path can be either absolute or
-- relative and need not refer to an existing file.  The order of arguments
-- follows the POSIX convention.
--
-- To remove an existing file symbolic link, use 'removeFile'.
--
-- Although the distinction between /file/ symbolic links and /directory/
-- symbolic links does not exist on POSIX systems, on Windows this is an
-- intrinsic property of every symbolic link and cannot be changed without
-- recreating the link.  A file symbolic link that actually points to a
-- directory will fail to dereference and vice versa.  Moreover, creating
-- symbolic links on Windows may require privileges unavailable to users
-- outside the Administrators group.  Portable programs that use symbolic
-- links should take both into consideration.
--
-- On Windows, the function is implemented using @CreateSymbolicLink@.  Since
-- 1.3.3.0, the @SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE@ flag is included
-- if supported by the operating system.  On POSIX, the function uses @symlink@
-- and is therefore atomic.
--
-- Windows-specific errors: This operation may fail with 'permissionErrorType'
-- if the user lacks the privileges to create symbolic links.  It may also
-- fail with 'illegalOperationErrorType' if the file system does not support
-- symbolic links.
createFileLink
  :: OsPath                           -- ^ path to the target file
  -> OsPath                           -- ^ path of the link to be created
  -> IO ()
createFileLink target link =
  (`ioeAddLocation` "createFileLink") `modifyIOError` do
    createSymbolicLink False target link

-- | Create a /directory/ symbolic link.  The target path can be either
-- absolute or relative and need not refer to an existing directory.  The
-- order of arguments follows the POSIX convention.
--
-- To remove an existing directory symbolic link, use 'removeDirectoryLink'.
--
-- Although the distinction between /file/ symbolic links and /directory/
-- symbolic links does not exist on POSIX systems, on Windows this is an
-- intrinsic property of every symbolic link and cannot be changed without
-- recreating the link.  A file symbolic link that actually points to a
-- directory will fail to dereference and vice versa.  Moreover, creating
-- symbolic links on Windows may require privileges unavailable to users
-- outside the Administrators group.  Portable programs that use symbolic
-- links should take both into consideration.
--
-- On Windows, the function is implemented using @CreateSymbolicLink@ with
-- @SYMBOLIC_LINK_FLAG_DIRECTORY@.  Since 1.3.3.0, the
-- @SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE@ flag is also included if
-- supported by the operating system.   On POSIX, this is an alias for
-- 'createFileLink' and is therefore atomic.
--
-- Windows-specific errors: This operation may fail with 'permissionErrorType'
-- if the user lacks the privileges to create symbolic links.  It may also
-- fail with 'illegalOperationErrorType' if the file system does not support
-- symbolic links.
createDirectoryLink
  :: OsPath                           -- ^ path to the target directory
  -> OsPath                           -- ^ path of the link to be created
  -> IO ()
createDirectoryLink target link =
  (`ioeAddLocation` "createDirectoryLink") `modifyIOError` do
    createSymbolicLink True target link

-- | Remove an existing /directory/ symbolic link.
--
-- On Windows, this is an alias for 'removeDirectory'.  On POSIX systems, this
-- is an alias for 'removeFile'.
--
-- See also: 'removeFile', which can remove an existing /file/ symbolic link.
removeDirectoryLink :: OsPath -> IO ()
removeDirectoryLink path =
  (`ioeAddLocation` "removeDirectoryLink") `modifyIOError` do
    removePathInternal linkToDirectoryIsDirectory path

-- | Check whether an existing @path@ is a symbolic link.  If @path@ is a
-- regular file or directory, 'False' is returned.  If @path@ does not exist
-- or is otherwise inaccessible, an exception is thrown (see below).
--
-- On Windows, this checks for @FILE_ATTRIBUTE_REPARSE_POINT@.  In addition to
-- symbolic links, the function also returns true on junction points.  On
-- POSIX systems, this checks for @S_IFLNK@.
--
-- The operation may fail with:
--
-- * 'isDoesNotExistError' if the symbolic link does not exist; or
--
-- * 'isPermissionError' if the user is not permitted to read the symbolic
--   link.
pathIsSymbolicLink :: OsPath -> IO Bool
pathIsSymbolicLink path =
  ((`ioeAddLocation` "pathIsSymbolicLink") .
   (`ioeSetOsPath` path)) `modifyIOError` do
     fileTypeIsLink . fileTypeFromMetadata <$> getSymbolicLinkMetadata path

-- | Retrieve the target path of either a file or directory symbolic link.
-- The returned path may not be absolute, may not exist, and may not even be a
-- valid path.
--
-- On Windows systems, this calls @DeviceIoControl@ with
-- @FSCTL_GET_REPARSE_POINT@.  In addition to symbolic links, the function
-- also works on junction points.  On POSIX systems, this calls @readlink@.
--
-- Windows-specific errors: This operation may fail with
-- 'illegalOperationErrorType' if the file system does not support symbolic
-- links.
getSymbolicLinkTarget :: OsPath -> IO OsPath
getSymbolicLinkTarget path =
  (`ioeAddLocation` "getSymbolicLinkTarget") `modifyIOError` do
    readSymbolicLink path

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
getAccessTime :: OsPath -> IO UTCTime
getAccessTime path =
  (`ioeAddLocation` "getAccessTime") `modifyIOError` do
    accessTimeFromMetadata <$> getFileMetadata (emptyToCurDir path)

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
getModificationTime :: OsPath -> IO UTCTime
getModificationTime path =
  (`ioeAddLocation` "getModificationTime") `modifyIOError` do
    modificationTimeFromMetadata <$> getFileMetadata (emptyToCurDir path)

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
setAccessTime :: OsPath -> UTCTime -> IO ()
setAccessTime path atime =
  (`ioeAddLocation` "setAccessTime") `modifyIOError` do
    setFileTimes path (Just atime, Nothing)

-- | Change the time at which the file or directory was last modified.
--
-- The operation may fail with:
--
-- * 'isPermissionError' if the user is not permitted to alter the
--   modification time; or
--
-- * 'isDoesNotExistError' if the file or directory does not exist.
--
-- * 'InvalidArgument' on FAT32 file system if the time is before
--   DOS Epoch (1 January 1980).
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
setModificationTime :: OsPath -> UTCTime -> IO ()
setModificationTime path mtime =
  (`ioeAddLocation` "setModificationTime") `modifyIOError` do
    setFileTimes path (Nothing, Just mtime)

setFileTimes :: OsPath -> (Maybe UTCTime, Maybe UTCTime) -> IO ()
setFileTimes _ (Nothing, Nothing) = return ()
setFileTimes path (atime, mtime) =
  ((`ioeAddLocation` "setFileTimes") .
   (`ioeSetOsPath` path)) `modifyIOError` do
    setTimes (emptyToCurDir path)
             (utcTimeToPOSIXSeconds <$> atime, utcTimeToPOSIXSeconds <$> mtime)

{- | Returns the current user's home directory.

The directory returned is expected to be writable by the current user,
but note that it isn't generally considered good practice to store
application-specific data here; use 'getXdgDirectory' or
'getAppUserDataDirectory' instead.

On Unix, 'getHomeDirectory' behaves as follows:

* Returns $HOME env variable if set (including to an empty string).
* Otherwise uses home directory returned by `getpwuid_r` using the UID of the current proccesses user. This basically reads the /etc/passwd file. An empty home directory field is considered valid.

On Windows, the system is queried for a suitable path; a typical path might be @C:\/Users\//\<user\>/@.

The operation may fail with:

* @UnsupportedOperation@
The operating system has no notion of home directory.

* 'isDoesNotExistError'
The home directory for the current user does not exist, or
cannot be found.
-}
getHomeDirectory :: IO OsPath
getHomeDirectory =
  (`ioeAddLocation` "getHomeDirectory") `modifyIOError` do
    getHomeDirectoryInternal

-- | Obtain the paths to special directories for storing user-specific
--   application data, configuration, and cache files, conforming to the
--   <http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html XDG Base Directory Specification>.
--   Compared with 'getAppUserDataDirectory', this function provides a more
--   fine-grained hierarchy as well as greater flexibility for the user.
--
--   On Windows, 'XdgData' and 'XdgConfig' usually map to the same directory
--   unless overridden.
--
--   Refer to the docs of 'XdgDirectory' for more details.
--
--   The second argument is usually the name of the application.  Since it
--   will be integrated into the path, it must consist of valid path
--   characters.  Note: if the second argument is an absolute path, it will
--   just return the second argument.
--
--   Note: The directory may not actually exist, in which case you would need
--   to create it with file mode @700@ (i.e. only accessible by the owner).
--
--   As of 1.3.5.0, the environment variable is ignored if set to a relative
--   path, per revised XDG Base Directory Specification.  See
--   <https://github.com/haskell/directory/issues/100 #100>.
getXdgDirectory :: XdgDirectory         -- ^ which special directory
                -> OsPath             -- ^ a relative path that is appended
                                        --   to the path; if empty, the base
                                        --   path is returned
                -> IO OsPath
getXdgDirectory xdgDir suffix =
  (`ioeAddLocation` "getXdgDirectory") `modifyIOError` do
    simplify . (</> suffix) <$> do
      env <- lookupEnvOs . os $ case xdgDir of
        XdgData   -> "XDG_DATA_HOME"
        XdgConfig -> "XDG_CONFIG_HOME"
        XdgCache  -> "XDG_CACHE_HOME"
        XdgState  -> "XDG_STATE_HOME"
      case env of
        Just path | isAbsolute path -> pure path
        _                           -> getXdgDirectoryFallback getHomeDirectory xdgDir

-- | Similar to 'getXdgDirectory' but retrieves the entire list of XDG
-- directories.
--
-- On Windows, 'XdgDataDirs' and 'XdgConfigDirs' usually map to the same list
-- of directories unless overridden.
--
-- Refer to the docs of 'XdgDirectoryList' for more details.
getXdgDirectoryList :: XdgDirectoryList -- ^ which special directory list
                    -> IO [OsPath]
getXdgDirectoryList xdgDirs =
  (`ioeAddLocation` "getXdgDirectoryList") `modifyIOError` do
    env <- lookupEnvOs . os $ case xdgDirs of
      XdgDataDirs   -> "XDG_DATA_DIRS"
      XdgConfigDirs -> "XDG_CONFIG_DIRS"
    case env of
      Nothing    -> getXdgDirectoryListFallback xdgDirs
      Just paths -> pure (splitSearchPath paths)

-- | Obtain the path to a special directory for storing user-specific
--   application data (traditional Unix location).  Newer applications may
--   prefer the the XDG-conformant location provided by 'getXdgDirectory'
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
--   * @UnsupportedOperation@
--     The operating system has no notion of application-specific data
--     directory.
--
--   * 'isDoesNotExistError'
--     The home directory for the current user does not exist, or cannot be
--     found.
--
getAppUserDataDirectory :: OsPath     -- ^ a relative path that is appended
                                        --   to the path
                        -> IO OsPath
getAppUserDataDirectory appName = do
  (`ioeAddLocation` "getAppUserDataDirectory") `modifyIOError` do
    getAppUserDataDirectoryInternal appName

{- | Returns the current user's document directory.

The directory returned is expected to be writable by the current user,
but note that it isn't generally considered good practice to store
application-specific data here; use 'getXdgDirectory' or
'getAppUserDataDirectory' instead.

On Unix, 'getUserDocumentsDirectory' returns the value of the @HOME@
environment variable.  On Windows, the system is queried for a
suitable path; a typical path might be @C:\/Users\//\<user\>/\/Documents@.

The operation may fail with:

* @UnsupportedOperation@
The operating system has no notion of document directory.

* 'isDoesNotExistError'
The document directory for the current user does not exist, or
cannot be found.
-}
getUserDocumentsDirectory :: IO OsPath
getUserDocumentsDirectory = do
  (`ioeAddLocation` "getUserDocumentsDirectory") `modifyIOError` do
    getUserDocumentsDirectoryInternal

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

* @UnsupportedOperation@
The operating system has no notion of temporary directory.

The function doesn\'t verify whether the path exists.
-}
getTemporaryDirectory :: IO OsPath
getTemporaryDirectory = getTemporaryDirectoryInternal

-- | Get the contents of the @PATH@ environment variable.
getExecSearchPath :: IO [OsPath]
getExecSearchPath = getPath
