{-# OPTIONS_GHC -w #-}
-- XXX We get some warnings on Windows
#ifdef __GLASGOW_HASKELL__
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
    , renameDirectory

    , getDirectoryContents
    , getCurrentDirectory
    , setCurrentDirectory

    -- * Pre-defined directories
    , getHomeDirectory
    , getAppUserDataDirectory
    , getUserDocumentsDirectory
    , getTemporaryDirectory

    -- * Actions on files
    , removeFile
    , renameFile
    , copyFile
    
    , canonicalizePath
    , makeRelativeToCurrentDirectory
    , findExecutable
    , findFile

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

    , getModificationTime
   ) where

import Control.Monad (guard)
import System.Environment      ( getEnv )
import System.FilePath
import System.IO
import System.IO.Error
import Control.Monad           ( when, unless )
import Control.Exception.Base as E

#ifdef __HUGS__
import Hugs.Directory
#endif /* __HUGS__ */

import Foreign
import Foreign.C

{-# CFILES cbits/directory.c #-}

import Data.Time
import Data.Time.Clock.POSIX

#ifdef __GLASGOW_HASKELL__

import GHC.IO.Exception ( IOException(..), IOErrorType(..), ioException )
import GHC.IO.Encoding
import GHC.Foreign as GHC

#ifdef mingw32_HOST_OS
import System.Posix.Types
import System.Posix.Internals
import qualified System.Win32 as Win32
#else
import qualified System.Posix as Posix
#endif

#endif /* __GLASGOW_HASKELL__ */

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
  withFilePath name $ \s -> do
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
    throwErrnoIfMinus1_ "setPermissions" $ do
      c_stat p_name p_stat
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
    createDirs (dir:[])   = createDir dir throw
    createDirs (dir:dirs) =
      createDir dir $ \_ -> do
        createDirs dirs
        createDir dir throw

    createDir :: FilePath -> (IOException -> IO ()) -> IO ()
    createDir dir notExistHandler = do
      r <- E.try $ createDirectory dir
      case (r :: Either IOException ()) of
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
         || isPermissionError e -> (do
#ifdef mingw32_HOST_OS
              withFileStatus "createDirectoryIfMissing" dir $ \st -> do
                 isDir <- isDirectory st
                 if isDir then return ()
                          else throw e
#else
              stat <- Posix.getFileStatus dir
              if Posix.isDirectory stat
                 then return ()
                 else throw e
#endif
              ) `E.catch` ((\_ -> return ()) :: IOException -> IO ())
          | otherwise              -> throw e

#if __GLASGOW_HASKELL__
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
EIO

* 'InvalidArgument'
The operand is not a valid directory name.
[ENAMETOOLONG, ELOOP]

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

-- | @'removeDirectoryRecursive' dir@  removes an existing directory /dir/
-- together with its content and all subdirectories. Be careful, 
-- if the directory contains symlinks, the function will follow them.
removeDirectoryRecursive :: FilePath -> IO ()
removeDirectoryRecursive startLoc = do
  cont <- getDirectoryContents startLoc
  sequence_ [rm (startLoc </> x) | x <- cont, x /= "." && x /= ".."]
  removeDirectory startLoc
  where
    rm :: FilePath -> IO ()
    rm f = do temp <- E.try (removeFile f)
              case temp of
                Left e  -> do isDir <- doesDirectoryExist f
                              -- If f is not a directory, re-throw the error
                              unless isDir $ throw (e :: SomeException)
                              removeDirectoryRecursive f
                Right _ -> return ()

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
        then ioException (ioeSetErrorString
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
renameFile opath npath = do
   -- XXX this test isn't performed atomically with the following rename
#ifdef mingw32_HOST_OS
   -- ToDo: use Win32 API
   withFileOrSymlinkStatus "renameFile" opath $ \st -> do
   is_dir <- isDirectory st
#else
   stat <- Posix.getSymbolicLinkStatus opath
   let is_dir = Posix.isDirectory stat
#endif
   if is_dir
        then ioException (ioeSetErrorString
                          (mkIOError InappropriateType "renameFile" Nothing (Just opath))
                          "is a directory")
        else do
#ifdef mingw32_HOST_OS
   Win32.moveFileEx opath npath Win32.mOVEFILE_REPLACE_EXISTING
#else
   Posix.rename opath npath
#endif

#endif /* __GLASGOW_HASKELL__ */

{- |@'copyFile' old new@ copies the existing file from /old/ to /new/.
If the /new/ file already exists, it is atomically replaced by the /old/ file.
Neither path may refer to an existing directory.  The permissions of /old/ are
copied to /new/, if possible.
-}

copyFile :: FilePath -> FilePath -> IO ()
copyFile fromFPath toFPath =
    copy `catchIOError` (\exc -> throw $ ioeSetLocation exc "copyFile")
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

-- | Given a path referring to a file or directory, returns a
-- canonicalized path. The intent is that two paths referring
-- to the same file\/directory will map to the same canonicalized
-- path.
--
-- Note that it is impossible to guarantee that the
-- implication (same file\/dir \<=\> same canonicalizedPath) holds
-- in either direction: this function can make only a best-effort
-- attempt.
--
-- The precise behaviour is that of the C realpath function
-- GetFullPathNameW on Windows). In particular, the behaviour
-- on paths that do not exist is known to vary from platform
-- to platform. Some platforms do not alter the input, some
-- do, and on some an exception will be thrown.
canonicalizePath :: FilePath -> IO FilePath
canonicalizePath fpath =
#if defined(mingw32_HOST_OS)
         do path <- Win32.getFullPathName fpath
#else
  do enc <- getFileSystemEncoding
     GHC.withCString enc fpath $ \pInPath ->
       allocaBytes long_path_size $ \pOutPath ->
         do throwErrnoPathIfNull "canonicalizePath" fpath $ c_realpath pInPath pOutPath
            path <- GHC.peekCString enc pOutPath
#endif
            return (normalise path)
        -- normalise does more stuff, like upper-casing the drive letter

#if !defined(mingw32_HOST_OS)
foreign import ccall unsafe "realpath"
                   c_realpath :: CString
                              -> CString
                              -> IO CString
#endif

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
findExecutable binary =
#if defined(mingw32_HOST_OS)
  Win32.searchPath Nothing binary ('.':exeExtension)
#else
 do
  path <- getEnv "PATH"
  findFile (splitSearchPath path) (binary <.> exeExtension)
#endif

-- | Search through the given set of directories for the given file.
-- Used by 'findExecutable' on non-windows platforms.
findFile :: [FilePath] -> String -> IO (Maybe FilePath)
findFile paths fileName = search paths
  where
    search :: [FilePath] -> IO (Maybe FilePath)
    search [] = return Nothing
    search (d:ds) = do
        let path = d </> fileName
        b <- doesFileExist path
        if b then return (Just path)
             else search ds

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
      loop
 where
  loop dirp = do
     e <- Posix.readDirStream dirp
     if null e then return [] else do
       es <- loop dirp
       return (e:es)
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

#endif /* __GLASGOW_HASKELL__ */

#ifdef __GLASGOW_HASKELL__
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
   `E.catch` ((\ _ -> return False) :: IOException -> IO Bool)

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
   `E.catch` ((\ _ -> return False) :: IOException -> IO Bool)

{- |The 'getModificationTime' operation returns the
clock time at which the file or directory was last modified.

The operation may fail with:

* 'isPermissionError' if the user is not permitted to access
  the modification time; or

* 'isDoesNotExistError' if the file or directory does not exist.

-}

getModificationTime :: FilePath -> IO UTCTime
getModificationTime name = do
#ifdef mingw32_HOST_OS
 -- ToDo: use Win32 API so we can get sub-second resolution
 withFileStatus "getModificationTime" name $ \ st -> do
 modificationTime st
#else
  stat <- Posix.getFileStatus name
  let mod_time :: POSIXTime
      mod_time = Posix.modificationTimeHiRes stat
  return $ posixSecondsToUTCTime mod_time
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

withFileOrSymlinkStatus :: String -> FilePath -> (Ptr CStat -> IO a) -> IO a
withFileOrSymlinkStatus loc name f = do
  modifyIOError (`ioeSetFileName` name) $
    allocaBytes sizeof_stat $ \p ->
      withFilePath name $ \s -> do
        throwErrnoIfMinus1Retry_ loc (lstat s p)
        f p

modificationTime :: Ptr CStat -> IO UTCTime
modificationTime stat = do
    mtime <- st_mtime stat
    return $ posixSecondsToUTCTime $ realToFrac (mtime :: CTime)

isDirectory :: Ptr CStat -> IO Bool
isDirectory stat = do
  mode <- st_mode stat
  return (s_isdir mode)

fileNameEndClean :: String -> String
fileNameEndClean name = if isDrive name then addTrailingPathSeparator name
                                        else dropTrailingPathSeparator name

foreign import ccall unsafe "HsDirectory.h __hscore_S_IRUSR" s_IRUSR :: CMode
foreign import ccall unsafe "HsDirectory.h __hscore_S_IWUSR" s_IWUSR :: CMode
foreign import ccall unsafe "HsDirectory.h __hscore_S_IXUSR" s_IXUSR :: CMode
foreign import ccall unsafe "__hscore_S_IFDIR" s_IFDIR :: CMode
#endif


#ifdef __GLASGOW_HASKELL__
foreign import ccall unsafe "__hscore_long_path_size"
  long_path_size :: Int
#else
long_path_size :: Int
long_path_size = 2048   --  // guess?
#endif /* __GLASGOW_HASKELL__ */

{- | Returns the current user's home directory.

The directory returned is expected to be writable by the current user,
but note that it isn't generally considered good practice to store
application-specific data here; use 'getAppUserDataDirectory'
instead.

On Unix, 'getHomeDirectory' returns the value of the @HOME@
environment variable.  On Windows, the system is queried for a
suitable path; a typical path might be 
@C:\/Documents And Settings\/user@.

The operation may fail with:

* 'UnsupportedOperation'
The operating system has no notion of home directory.

* 'isDoesNotExistError'
The home directory for the current user does not exist, or
cannot be found.
-}
getHomeDirectory :: IO FilePath
getHomeDirectory =
  modifyIOError ((`ioeSetLocation` "getHomeDirectory")) $ do
#if defined(mingw32_HOST_OS)
    r <- E.try $ Win32.sHGetFolderPath nullPtr Win32.cSIDL_PROFILE nullPtr 0
    case (r :: Either IOException String) of
      Right s -> return s
      Left  _ -> do
        r1 <- E.try $ Win32.sHGetFolderPath nullPtr Win32.cSIDL_WINDOWS nullPtr 0
        case r1 of
          Right s -> return s
          Left  e -> ioError (e :: IOException)
#else
    getEnv "HOME"
#endif

{- | Returns the pathname of a directory in which application-specific
data for the current user can be stored.  The result of
'getAppUserDataDirectory' for a given application is specific to
the current user.

The argument should be the name of the application, which will be used
to construct the pathname (so avoid using unusual characters that
might result in an invalid pathname).

Note: the directory may not actually exist, and may need to be created
first.  It is expected that the parent directory exists and is
writable.

On Unix, this function returns @$HOME\/.appName@.  On Windows, a
typical path might be 

> C:/Documents And Settings/user/Application Data/appName

The operation may fail with:

* 'UnsupportedOperation'
The operating system has no notion of application-specific data directory.

* 'isDoesNotExistError'
The home directory for the current user does not exist, or
cannot be found.
-}
getAppUserDataDirectory :: String -> IO FilePath
getAppUserDataDirectory appName = do
  modifyIOError ((`ioeSetLocation` "getAppUserDataDirectory")) $ do
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
application-specific data here; use 'getAppUserDataDirectory'
instead.

On Unix, 'getUserDocumentsDirectory' returns the value of the @HOME@
environment variable.  On Windows, the system is queried for a
suitable path; a typical path might be 
@C:\/Documents And Settings\/user\/My Documents@.

The operation may fail with:

* 'UnsupportedOperation'
The operating system has no notion of document directory.

* 'isDoesNotExistError'
The document directory for the current user does not exist, or
cannot be found.
-}
getUserDocumentsDirectory :: IO FilePath
getUserDocumentsDirectory = do
  modifyIOError ((`ioeSetLocation` "getUserDocumentsDirectory")) $ do
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
getTemporaryDirectory = do
#if defined(mingw32_HOST_OS)
  Win32.getTemporaryDirectory
#else
  getEnv "TMPDIR"
    `catchIOError` \e -> if isDoesNotExistError e then return "/tmp"
                          else throw e
#endif

-- ToDo: This should be determined via autoconf (AC_EXEEXT)
-- | Extension for executable files
-- (typically @\"\"@ on Unix and @\"exe\"@ on Windows or OS\/2)
exeExtension :: String
#ifdef mingw32_HOST_OS
exeExtension = "exe"
#else
exeExtension = ""
#endif
