module System.Directory.Internal.Common where

data FileType = File
              | SymbolicLink -- ^ POSIX: either file or directory link; Windows: file link
              | Directory
              | DirectoryLink -- ^ Windows only
              deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Check whether the given 'FileType' is considered a directory by the
-- operating system.  This affects the choice of certain functions
-- e.g. `removeDirectory` vs `removeFile`.
fileTypeIsDirectory :: FileType -> Bool
fileTypeIsDirectory Directory     = True
fileTypeIsDirectory DirectoryLink = True
fileTypeIsDirectory _             = False

data Permissions
  = Permissions
  { readable :: Bool
  , writable :: Bool
  , executable :: Bool
  , searchable :: Bool
  } deriving (Eq, Ord, Read, Show)
