{-# LANGUAGE CPP #-}

#if !MIN_VERSION_base(4, 8, 0)
-- In base-4.8.0 the Foreign module became Safe
{-# LANGUAGE Trustworthy #-}
#endif

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

    -- * Actions on files
    , removeFile
    , renameFile
    , renamePath
    , copyFile
    , copyFileWithMetadata
    , getFileSize

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

    -- * Deprecated
    , isSymbolicLink

   ) where
import Prelude ()
import System.Directory.Internal
import System.Directory.Internal.Prelude
import System.FilePath
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
  )
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

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
getPermissions :: FilePath -> IO Permissions
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
setPermissions :: FilePath -> Permissions -> IO ()
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
copyPermissions :: FilePath -> FilePath -> IO ()
copyPermissions src dst =
  (`ioeAddLocation` "copyPermissions") `modifyIOError` do
    m <- getFileMetadata src
    copyPermissionsFromMetadata m dst

copyPermissionsFromMetadata :: Metadata -> FilePath -> IO ()
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

createDirectory :: FilePath -> IO ()
createDirectory = createDirectoryInternal

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

removeDirectory :: FilePath -> IO ()
removeDirectory = removePathInternal True

-- | @'removeDirectoryRecursive' dir@ removes an existing directory /dir/
-- together with its contents and subdirectories. Within this directory,
-- symbolic links are removed without affecting their targets.
--
-- On Windows, the operation fails if /dir/ is a directory symbolic link.
--
-- This operation is reported to be flaky on Windows so retry logic may 
-- be advisable. See: https://github.com/haskell/directory/pull/108
removeDirectoryRecursive :: FilePath -> IO ()
removeDirectoryRecursive path =
  (`ioeAddLocation` "removeDirectoryRecursive") `modifyIOError` do
    m <- getSymbolicLinkMetadata path
    case fileTypeFromMetadata m of
      Directory ->
        removeContentsRecursive path
      DirectoryLink ->
        ioError (err `ioeSetErrorString` "is a directory symbolic link")
      _ ->
        ioError (err `ioeSetErrorString` "not a directory")
  where err = mkIOError InappropriateType "" Nothing (Just path)

-- | @removePathRecursive path@ removes an existing file or directory at
-- /path/ together with its contents and subdirectories. Symbolic links are
-- removed without affecting their the targets.
--
-- This operation is reported to be flaky on Windows so retry logic may 
-- be advisable. See: https://github.com/haskell/directory/pull/108
removePathRecursive :: FilePath -> IO ()
removePathRecursive path =
  (`ioeAddLocation` "removePathRecursive") `modifyIOError` do
    m <- getSymbolicLinkMetadata path
    case fileTypeFromMetadata m of
      Directory     -> removeContentsRecursive path
      DirectoryLink -> removeDirectory path
      _             -> removeFile path

-- | @removeContentsRecursive dir@ removes the contents of the directory
-- /dir/ recursively. Symbolic links are removed without affecting their the
-- targets.
--
-- This operation is reported to be flaky on Windows so retry logic may 
-- be advisable. See: https://github.com/haskell/directory/pull/108
removeContentsRecursive :: FilePath -> IO ()
removeContentsRecursive path =
  (`ioeAddLocation` "removeContentsRecursive") `modifyIOError` do
    cont <- listDirectory path
    traverse_ removePathRecursive [path </> x | x <- cont]
    removeDirectory path

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
--
-- @since 1.2.7.0
removePathForcibly :: FilePath -> IO ()
removePathForcibly path =
  (`ioeAddLocation` "removePathForcibly") `modifyIOError` do
    makeRemovable path `catchIOError` \ _ -> pure ()
    ignoreDoesNotExistError $ do
      m <- getSymbolicLinkMetadata path
      case fileTypeFromMetadata m of
        DirectoryLink -> removeDirectory path
        Directory     -> do
          names <- listDirectory path
          sequenceWithIOErrors_ $
            [ removePathForcibly (path </> name) | name <- names ] ++
            [ removeDirectory path ]
        _             -> removeFile path
  where

    ignoreDoesNotExistError :: IO () -> IO ()
    ignoreDoesNotExistError action =
      () <$ tryIOErrorType isDoesNotExistError action

    makeRemovable :: FilePath -> IO ()
    makeRemovable p = do
      perms <- getPermissions p
      setPermissions path perms{ readable = True
                               , searchable = True
                               , writable = True }

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

removeFile :: FilePath -> IO ()
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

renameDirectory :: FilePath -> FilePath -> IO ()
renameDirectory opath npath =
   (`ioeAddLocation` "renameDirectory") `modifyIOError` do
     -- XXX this test isn't performed atomically with the following rename
     isDir <- pathIsDirectory opath
     when (not isDir) $ do
       ioError . (`ioeSetErrorString` "not a directory") $
         (mkIOError InappropriateType "renameDirectory" Nothing (Just opath))
     renamePath opath npath

{- |@'renameFile' old new@ changes the name of an existing file system
object from /old/ to /new/.  If the /new/ object already
exists, it is atomically replaced by the /old/ object.  Neither
path may refer to an existing directory.  A conformant implementation
need not support renaming files in all situations (e.g. renaming
across different physical devices), but the constraints must be
documented.

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

renameFile :: FilePath -> FilePath -> IO ()
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
            Right True -> ioError . (`ioeSetErrorString` "is a directory") $
                          mkIOError InappropriateType "" Nothing (Just path)
            _          -> pure ()

-- | Rename a file or directory.  If the destination path already exists, it
-- is replaced atomically.  The destination path must not point to an existing
-- directory.  A conformant implementation need not support renaming files in
-- all situations (e.g. renaming across different physical devices), but the
-- constraints must be documented.
--
-- The operation may fail with:
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
-- @since 1.2.7.0
renamePath :: FilePath                  -- ^ Old path
           -> FilePath                  -- ^ New path
           -> IO ()
renamePath opath npath =
  (`ioeAddLocation` "renamePath") `modifyIOError` do
    renamePathInternal opath npath

-- | Copy a file with its permissions.  If the destination file already exists,
-- it is replaced atomically.  Neither path may refer to an existing
-- directory.  No exceptions are thrown if the permissions could not be
-- copied.
copyFile :: FilePath                    -- ^ Source filename
         -> FilePath                    -- ^ Destination filename
         -> IO ()
copyFile fromFPath toFPath =
  (`ioeAddLocation` "copyFile") `modifyIOError` do
    atomicCopyFileContents fromFPath toFPath
      (ignoreIOExceptions . copyPermissions fromFPath)

-- | Copy the contents of a source file to a destination file, replacing the
-- destination file atomically via @withReplacementFile@, resetting the
-- attributes of the destination file to the defaults.
atomicCopyFileContents :: FilePath            -- ^ Source filename
                       -> FilePath            -- ^ Destination filename
                       -> (FilePath -> IO ()) -- ^ Post-action
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
withReplacementFile :: FilePath            -- ^ Destination file
                    -> (FilePath -> IO ()) -- ^ Post-action
                    -> (Handle -> IO a)    -- ^ Main action
                    -> IO a
withReplacementFile path postAction action =
  (`ioeAddLocation` "withReplacementFile") `modifyIOError` do
    mask $ \ restore -> do
      (tmpFPath, hTmp) <- openBinaryTempFile (takeDirectory path)
                                             ".copyFile.tmp"
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
--
-- @since 1.2.6.0
copyFileWithMetadata :: FilePath        -- ^ Source file
                     -> FilePath        -- ^ Destination file
                     -> IO ()
copyFileWithMetadata src dst =
  (`ioeAddLocation` "copyFileWithMetadata") `modifyIOError`
    copyFileWithMetadataInternal copyPermissionsFromMetadata
                                 copyTimesFromMetadata
                                 src
                                 dst

copyTimesFromMetadata :: Metadata -> FilePath -> IO ()
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
canonicalizePath :: FilePath -> IO FilePath
canonicalizePath = \ path ->
  ((`ioeAddLocation` "canonicalizePath") .
   (`ioeSetFileName` path)) `modifyIOError` do
    -- simplify does more stuff, like upper-casing the drive letter
    dropTrailingPathSeparator . simplify <$>
      (canonicalizePathWith attemptRealpath =<< prependCurrentDirectory path)
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
        suffixes = tail (scanr (</>) "" segments)

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
-- @since 1.2.2.0
--
makeAbsolute :: FilePath -> IO FilePath
makeAbsolute path =
  ((`ioeAddLocation` "makeAbsolute") .
   (`ioeSetFileName` path)) `modifyIOError` do
    matchTrailingSeparator path . simplify <$> prependCurrentDirectory path

-- | Add or remove the trailing path separator in the second path so as to
-- match its presence in the first path.
--
-- (internal API)
matchTrailingSeparator :: FilePath -> FilePath -> FilePath
matchTrailingSeparator path
  | hasTrailingPathSeparator path = addTrailingPathSeparator
  | otherwise                     = dropTrailingPathSeparator

-- | Construct a path relative to the current directory, similar to
-- 'makeRelative'.
--
-- The operation may fail with the same exceptions as 'getCurrentDirectory'.
makeRelativeToCurrentDirectory :: FilePath -> IO FilePath
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
findExecutable :: String -> IO (Maybe FilePath)
findExecutable binary =
  listTHead
    (findExecutablesLazyInternal findExecutablesInDirectoriesLazy binary)

-- | Search for executable files in a list of system-defined locations, which
-- generally includes @PATH@ and possibly more.
--
-- On Windows, this /only returns the first ocurrence/, if any.  Its behavior
-- is therefore equivalent to 'findExecutable'.
--
-- On non-Windows platforms, the behavior is equivalent to
-- 'findExecutablesInDirectories' using the search directories from the @PATH@
-- environment variable.  Details can be found in the documentation of
-- 'findExecutablesInDirectories'.
--
-- @since 1.2.2.0
findExecutables :: String -> IO [FilePath]
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
--
-- @since 1.2.4.0
findExecutablesInDirectories :: [FilePath] -> String -> IO [FilePath]
findExecutablesInDirectories path binary =
  listTToList (findExecutablesInDirectoriesLazy path binary)

findExecutablesInDirectoriesLazy :: [FilePath] -> String -> ListT IO FilePath
findExecutablesInDirectoriesLazy path binary =
  findFilesWithLazy isExecutable path (binary <.> exeExtension)

-- | Test whether a file has executable permissions.
isExecutable :: FilePath -> IO Bool
isExecutable file = executable <$> getPermissions file

-- | Search through the given list of directories for the given file.
--
-- The behavior is equivalent to 'findFileWith', returning only the first
-- occurrence.  Details can be found in the documentation of 'findFileWith'.
findFile :: [FilePath] -> String -> IO (Maybe FilePath)
findFile = findFileWith (\ _ -> pure True)

-- | Search through the given list of directories for the given file and
-- returns all paths where the given file exists.
--
-- The behavior is equivalent to 'findFilesWith'.  Details can be found in the
-- documentation of 'findFilesWith'.
--
-- @since 1.2.1.0
findFiles :: [FilePath] -> String -> IO [FilePath]
findFiles = findFilesWith (\ _ -> pure True)

-- | Search through a given list of directories for a file that has the given
-- name and satisfies the given predicate and return the path of the first
-- occurrence.  The directories are checked in a left-to-right order.
--
-- This is essentially a more performant version of 'findFilesWith' that
-- always returns the first result, if any.  Details can be found in the
-- documentation of 'findFilesWith'.
--
-- @since 1.2.6.0
findFileWith :: (FilePath -> IO Bool) -> [FilePath] -> String -> IO (Maybe FilePath)
findFileWith f ds name = listTHead (findFilesWithLazy f ds name)

-- | @findFilesWith predicate dirs name@ searches through the list of
-- directories (@dirs@) for files that have the given @name@ and satisfy the
-- given @predicate@ ands return the paths of those files.  The directories
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
--
-- @since 1.2.1.0
findFilesWith :: (FilePath -> IO Bool) -> [FilePath] -> String -> IO [FilePath]
findFilesWith f ds name = listTToList (findFilesWithLazy f ds name)

findFilesWithLazy
  :: (FilePath -> IO Bool) -> [FilePath] -> String -> ListT IO FilePath
findFilesWithLazy f dirs path
  -- make sure absolute paths are handled properly irrespective of 'dirs'
  -- https://github.com/haskell/directory/issues/72
  | isAbsolute path = ListT (find [""])
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
--
-- @since 1.2.4.0
exeExtension :: String
exeExtension = exeExtensionInternal

-- | Similar to 'listDirectory', but always includes the special entries (@.@
-- and @..@).  (This applies to Windows as well.)
--
-- The operation may fail with the same exceptions as 'listDirectory'.
getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents path =
  ((`ioeSetFileName` path) .
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
-- @since 1.2.5.0
--
listDirectory :: FilePath -> IO [FilePath]
listDirectory path = filter f <$> getDirectoryContents path
  where f filename = filename /= "." && filename /= ".."

-- | Obtain the current working directory as an absolute path.
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
getCurrentDirectory :: IO FilePath
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
setCurrentDirectory :: FilePath -> IO ()
setCurrentDirectory = setCurrentDirectoryInternal

-- | Run an 'IO' action with the given working directory and restore the
-- original working directory afterwards, even if the given action fails due
-- to an exception.
--
-- The operation may fail with the same exceptions as 'getCurrentDirectory'
-- and 'setCurrentDirectory'.
--
-- @since 1.2.3.0
--
withCurrentDirectory :: FilePath  -- ^ Directory to execute in
                     -> IO a      -- ^ Action to be executed
                     -> IO a
withCurrentDirectory dir action =
  bracket getCurrentDirectory setCurrentDirectory $ \ _ -> do
    setCurrentDirectory dir
    action

-- | Obtain the size of a file in bytes.
--
-- @since 1.2.7.0
getFileSize :: FilePath -> IO Integer
getFileSize path =
  (`ioeAddLocation` "getFileSize") `modifyIOError` do
    fileSizeFromMetadata <$> getFileMetadata path

-- | Test whether the given path points to an existing filesystem object.  If
-- the user lacks necessary permissions to search the parent directories, this
-- function may return false even if the file does actually exist.
--
-- @since 1.2.7.0
doesPathExist :: FilePath -> IO Bool
doesPathExist path = do
  (True <$ getFileMetadata path)
    `catchIOError` \ _ ->
      pure False

{- |The operation 'doesDirectoryExist' returns 'True' if the argument file
exists and is either a directory or a symbolic link to a directory,
and 'False' otherwise.
-}

doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist path = do
  pathIsDirectory path
    `catchIOError` \ _ ->
      pure False

{- |The operation 'doesFileExist' returns 'True'
if the argument file exists and is not a directory, and 'False' otherwise.
-}

doesFileExist :: FilePath -> IO Bool
doesFileExist path = do
  (not <$> pathIsDirectory path)
    `catchIOError` \ _ ->
      pure False

pathIsDirectory :: FilePath -> IO Bool
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
--
-- @since 1.3.1.0
createFileLink
  :: FilePath                           -- ^ path to the target file
  -> FilePath                           -- ^ path of the link to be created
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
--
-- @since 1.3.1.0
createDirectoryLink
  :: FilePath                           -- ^ path to the target directory
  -> FilePath                           -- ^ path of the link to be created
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
--
-- @since 1.3.1.0
removeDirectoryLink :: FilePath -> IO ()
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
--
-- @since 1.3.0.0
pathIsSymbolicLink :: FilePath -> IO Bool
pathIsSymbolicLink path =
  ((`ioeAddLocation` "pathIsSymbolicLink") .
   (`ioeSetFileName` path)) `modifyIOError` do
     fileTypeIsLink . fileTypeFromMetadata <$> getSymbolicLinkMetadata path

{-# DEPRECATED isSymbolicLink "Use 'pathIsSymbolicLink' instead" #-}
isSymbolicLink :: FilePath -> IO Bool
isSymbolicLink = pathIsSymbolicLink

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
--
-- @since 1.3.1.0
getSymbolicLinkTarget :: FilePath -> IO FilePath
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
-- @since 1.2.3.0
--
getAccessTime :: FilePath -> IO UTCTime
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
getModificationTime :: FilePath -> IO UTCTime
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
-- @since 1.2.3.0
--
setAccessTime :: FilePath -> UTCTime -> IO ()
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
setModificationTime path mtime =
  (`ioeAddLocation` "setModificationTime") `modifyIOError` do
    setFileTimes path (Nothing, Just mtime)

setFileTimes :: FilePath -> (Maybe UTCTime, Maybe UTCTime) -> IO ()
setFileTimes _ (Nothing, Nothing) = return ()
setFileTimes path (atime, mtime) =
  ((`ioeAddLocation` "setFileTimes") .
   (`ioeSetFileName` path)) `modifyIOError` do
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
getHomeDirectory :: IO FilePath
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
--
--   @since 1.2.3.0
getXdgDirectory :: XdgDirectory         -- ^ which special directory
                -> FilePath             -- ^ a relative path that is appended
                                        --   to the path; if empty, the base
                                        --   path is returned
                -> IO FilePath
getXdgDirectory xdgDir suffix =
  (`ioeAddLocation` "getXdgDirectory") `modifyIOError` do
    simplify . (</> suffix) <$> do
      env <- lookupEnv $ case xdgDir of
        XdgData   -> "XDG_DATA_HOME"
        XdgConfig -> "XDG_CONFIG_HOME"
        XdgCache  -> "XDG_CACHE_HOME"
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
                    -> IO [FilePath]
getXdgDirectoryList xdgDirs =
  (`ioeAddLocation` "getXdgDirectoryList") `modifyIOError` do
    env <- lookupEnv $ case xdgDirs of
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
getAppUserDataDirectory :: FilePath     -- ^ a relative path that is appended
                                        --   to the path
                        -> IO FilePath
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
getUserDocumentsDirectory :: IO FilePath
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
getTemporaryDirectory :: IO FilePath
getTemporaryDirectory = getTemporaryDirectoryInternal
