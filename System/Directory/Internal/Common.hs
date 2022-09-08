module System.Directory.Internal.Common
  ( module System.Directory.Internal.Common
  , OsPath
  , OsString
  ) where
import Prelude ()
import System.Directory.Internal.Prelude
import GHC.IO.Encoding.Failure (CodingFailureMode(TransliterateCodingFailure))
import GHC.IO.Encoding.UTF16 (mkUTF16le)
import GHC.IO.Encoding.UTF8 (mkUTF8)
import System.IO (hSetBinaryMode)
import System.OsPath
  ( OsPath
  , OsString
  , addTrailingPathSeparator
  , decodeUtf
  , decodeWith
  , encodeUtf
  , hasTrailingPathSeparator
  , isPathSeparator
  , isRelative
  , joinDrive
  , joinPath
  , normalise
  , pack
  , pathSeparator
  , pathSeparators
  , splitDirectories
  , splitDrive
  , toChar
  , unpack
  , unsafeFromChar
  )

-- | A generator with side-effects.
newtype ListT m a = ListT { unListT :: m (Maybe (a, ListT m a)) }

emptyListT :: Applicative m => ListT m a
emptyListT = ListT (pure Nothing)

maybeToListT :: Applicative m => m (Maybe a) -> ListT m a
maybeToListT m = ListT (((\ x -> (x, emptyListT)) <$>) <$> m)

listToListT :: Applicative m => [a] -> ListT m a
listToListT [] = emptyListT
listToListT (x : xs) = ListT (pure (Just (x, listToListT xs)))

liftJoinListT :: Monad m => m (ListT m a) -> ListT m a
liftJoinListT m = ListT (m >>= unListT)

listTHead :: Functor m => ListT m a -> m (Maybe a)
listTHead (ListT m) = (fst <$>) <$> m

listTToList :: Monad m => ListT m a -> m [a]
listTToList (ListT m) = do
  mx <- m
  case mx of
    Nothing -> return []
    Just (x, m') -> do
      xs <- listTToList m'
      return (x : xs)

andM :: Monad m => m Bool -> m Bool -> m Bool
andM mx my = do
  x <- mx
  if x
    then my
    else return x

sequenceWithIOErrors_ :: [IO ()] -> IO ()
sequenceWithIOErrors_ actions = go (Right ()) actions
  where

    go :: Either IOError () -> [IO ()] -> IO ()
    go (Left e)   []       = ioError e
    go (Right ()) []       = pure ()
    go s          (m : ms) = s `seq` do
      r <- tryIOError m
      go (s *> r) ms

-- | Similar to 'try' but only catches a specify kind of 'IOError' as
--   specified by the predicate.
tryIOErrorType :: (IOError -> Bool) -> IO a -> IO (Either IOError a)
tryIOErrorType check action = do
  result <- tryIOError action
  case result of
    Left  err -> if check err then pure (Left err) else throwIO err
    Right val -> pure (Right val)

-- | Attempt to perform the given action, silencing any IO exception thrown by
-- it.
ignoreIOExceptions :: IO () -> IO ()
ignoreIOExceptions io = io `catchIOError` (\_ -> pure ())

specializeErrorString :: String -> (IOError -> Bool) -> IO a -> IO a
specializeErrorString str errType action = do
  mx <- tryIOErrorType errType action
  case mx of
    Left  e -> throwIO (ioeSetErrorString e str)
    Right x -> pure x

ioeAddLocation :: IOError -> String -> IOError
ioeAddLocation e loc = do
  ioeSetLocation e newLoc
  where
    newLoc = loc <> if null oldLoc then "" else ":" <> oldLoc
    oldLoc = ioeGetLocation e

rightOrError :: Exception e => Either e a -> a
rightOrError (Left e)  = error (displayException e)
rightOrError (Right a) = a

-- | Fallibly converts String to OsString. Only intended to be used on literals.
os :: String -> OsString
os = rightOrError . encodeUtf

-- | Fallibly converts OsString to String. Only intended to be used on literals.
so :: OsString -> String
so = rightOrError . decodeUtf

ioeSetOsPath :: IOError -> OsPath -> IOError
ioeSetOsPath err =
  ioeSetFileName err .
  rightOrError .
  decodeWith
    (mkUTF8 TransliterateCodingFailure)
    (mkUTF16le TransliterateCodingFailure)

-- | Given a list of path segments, expand @.@ and @..@.  The path segments
-- must not contain path separators.
expandDots :: [OsPath] -> [OsPath]
expandDots = reverse . go []
  where
    go ys' xs' =
      case xs' of
        [] -> ys'
        x : xs
          | x == os "." -> go ys' xs
          | x == os ".." ->
              case ys' of
                [] -> go (x : ys') xs
                y : ys
                  | y == os ".." -> go (x : ys') xs
                  | otherwise -> go ys xs
          | otherwise -> go (x : ys') xs

-- | Convert to the right kind of slashes.
normalisePathSeps :: OsPath -> OsPath
normalisePathSeps p = pack (normaliseChar <$> unpack p)
  where normaliseChar c = if isPathSeparator c then pathSeparator else c

-- | Remove redundant trailing slashes and pick the right kind of slash.
normaliseTrailingSep :: OsPath -> OsPath
normaliseTrailingSep path = do
  let path' = reverse (unpack path)
  let (sep, path'') = span isPathSeparator path'
  let addSep = if null sep then id else (pathSeparator :)
  pack (reverse (addSep path''))

-- | Convert empty paths to the current directory, otherwise leave it
-- unchanged.
emptyToCurDir :: OsPath -> OsPath
emptyToCurDir path
  | path == mempty = os "."
  | otherwise      = path

-- | Similar to 'normalise' but empty paths stay empty.
simplifyPosix :: OsPath -> OsPath
simplifyPosix path
  | path == mempty = mempty
  | otherwise      = normalise path

-- | Similar to 'normalise' but:
--
-- * empty paths stay empty,
-- * parent dirs (@..@) are expanded, and
-- * paths starting with @\\\\?\\@ are preserved.
--
-- The goal is to preserve the meaning of paths better than 'normalise'.
simplifyWindows :: OsPath -> OsPath
simplifyWindows path
  | path == mempty         = mempty
  | drive' == os "\\\\?\\" = drive' <> subpath
  | otherwise              = simplifiedPath
  where
    simplifiedPath = joinDrive drive' subpath'
    (drive, subpath) = splitDrive path
    drive' = upperDrive (normaliseTrailingSep (normalisePathSeps drive))
    subpath' = appendSep . avoidEmpty . prependSep . joinPath .
               stripPardirs . expandDots . skipSeps .
               splitDirectories $ subpath

    upperDrive d = case unpack d of
      c : k : s
        | isAlpha (toChar c), toChar k == ':', all isPathSeparator s ->
          -- unsafeFromChar is safe here since all characters are ASCII.
          pack (unsafeFromChar (toUpper (toChar c)) : unsafeFromChar ':' : s)
      _ -> d
    skipSeps =
      (pack <$>) .
      filter (not . (`elem` (pure <$> pathSeparators))) .
      (unpack <$>)
    stripPardirs | pathIsAbsolute || subpathIsAbsolute = dropWhile (== os "..")
                 | otherwise = id
    prependSep | subpathIsAbsolute = (pack [pathSeparator] <>)
               | otherwise = id
    avoidEmpty | not pathIsAbsolute
               , drive == mempty || hasTrailingPathSep -- prefer "C:" over "C:."
                 = emptyToCurDir
               | otherwise = id
    appendSep p | hasTrailingPathSep, not (pathIsAbsolute && p == mempty)
                  = addTrailingPathSeparator p
                | otherwise = p
    pathIsAbsolute = not (isRelative path)
    subpathIsAbsolute = any isPathSeparator (take 1 (unpack subpath))
    hasTrailingPathSep = hasTrailingPathSeparator subpath

data FileType = File
              | SymbolicLink -- ^ POSIX: either file or directory link; Windows: file link
              | Directory
              | DirectoryLink -- ^ Windows only: directory link
              deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Check whether the given 'FileType' is considered a directory by the
-- operating system.  This affects the choice of certain functions
-- e.g. 'System.Directory.removeDirectory' vs 'System.Directory.removeFile'.
fileTypeIsDirectory :: FileType -> Bool
fileTypeIsDirectory Directory     = True
fileTypeIsDirectory DirectoryLink = True
fileTypeIsDirectory _             = False

-- | Return whether the given 'FileType' is a link.
fileTypeIsLink :: FileType -> Bool
fileTypeIsLink SymbolicLink  = True
fileTypeIsLink DirectoryLink = True
fileTypeIsLink _             = False

data Permissions
  = Permissions
  { readable :: Bool
  , writable :: Bool
  , executable :: Bool
  , searchable :: Bool
  } deriving (Eq, Ord, Read, Show)

withBinaryHandle :: IO Handle -> (Handle -> IO r) -> IO r
withBinaryHandle open = bracket openBinary hClose
  where
    openBinary = do
      h <- open
      hSetBinaryMode h True
      pure h

-- | Copy data from one handle to another until end of file.
copyHandleData :: Handle                -- ^ Source handle
               -> Handle                -- ^ Destination handle
               -> IO ()
copyHandleData hFrom hTo =
  (`ioeAddLocation` "copyData") `modifyIOError` do
    allocaBytes bufferSize go
  where
    bufferSize = 131072 -- 128 KiB, as coreutils `cp` uses as of May 2014 (see ioblksize.h)
    go buffer = do
      count <- hGetBuf hFrom buffer bufferSize
      when (count > 0) $ do
        hPutBuf hTo buffer count
        go buffer

-- | Special directories for storing user-specific application data,
-- configuration, and cache files, as specified by the
-- <http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html XDG Base Directory Specification>.
--
-- Note: On Windows, 'XdgData' and 'XdgConfig' usually map to the same
-- directory.
--
-- @since 1.2.3.0
data XdgDirectory
  = XdgData
    -- ^ For data files (e.g. images).
    -- It uses the @XDG_DATA_HOME@ environment variable.
    -- On non-Windows systems, the default is @~\/.local\/share@.
    -- On Windows, the default is @%APPDATA%@
    -- (e.g. @C:\/Users\//\<user\>/\/AppData\/Roaming@).
    -- Can be considered as the user-specific equivalent of @\/usr\/share@.
  | XdgConfig
    -- ^ For configuration files.
    -- It uses the @XDG_CONFIG_HOME@ environment variable.
    -- On non-Windows systems, the default is @~\/.config@.
    -- On Windows, the default is @%APPDATA%@
    -- (e.g. @C:\/Users\//\<user\>/\/AppData\/Roaming@).
    -- Can be considered as the user-specific equivalent of @\/etc@.
  | XdgCache
    -- ^ For non-essential files (e.g. cache).
    -- It uses the @XDG_CACHE_HOME@ environment variable.
    -- On non-Windows systems, the default is @~\/.cache@.
    -- On Windows, the default is @%LOCALAPPDATA%@
    -- (e.g. @C:\/Users\//\<user\>/\/AppData\/Local@).
    -- Can be considered as the user-specific equivalent of @\/var\/cache@.
  | XdgState
   -- ^ For data that should persist between (application) restarts,
   -- but that is not important or portable enough to the user that it
   -- should be stored in 'XdgData'.
   -- It uses the @XDG_STATE_HOME@ environment variable.
   -- On non-Windows sytems, the default is @~\/.local\/state@.  On
   -- Windows, the default is @%LOCALAPPDATA%@
   -- (e.g. @C:\/Users\//\<user\>/\/AppData\/Local@).
   --
   -- @since 1.3.7.0
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Search paths for various application data, as specified by the
-- <http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html XDG Base Directory Specification>.
--
-- The list of paths is split using 'System.FilePath.searchPathSeparator',
-- which on Windows is a semicolon.
--
-- Note: On Windows, 'XdgDataDirs' and 'XdgConfigDirs' usually yield the same
-- result.
--
-- @since 1.3.2.0
data XdgDirectoryList
  = XdgDataDirs
    -- ^ For data files (e.g. images).
    -- It uses the @XDG_DATA_DIRS@ environment variable.
    -- On non-Windows systems, the default is @\/usr\/local\/share\/@ and
    -- @\/usr\/share\/@.
    -- On Windows, the default is @%PROGRAMDATA%@ or @%ALLUSERSPROFILE%@
    -- (e.g. @C:\/ProgramData@).
  | XdgConfigDirs
    -- ^ For configuration files.
    -- It uses the @XDG_CONFIG_DIRS@ environment variable.
    -- On non-Windows systems, the default is @\/etc\/xdg@.
    -- On Windows, the default is @%PROGRAMDATA%@ or @%ALLUSERSPROFILE%@
    -- (e.g. @C:\/ProgramData@).
  deriving (Bounded, Enum, Eq, Ord, Read, Show)
