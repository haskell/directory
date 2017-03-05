module System.Directory.Internal.Common where

data FileType = File
              | SymbolicLink -- ^ POSIX: either file or directory link; Windows: file link
              | Directory
              | DirectoryLink -- ^ Windows only
              deriving (Bounded, Enum, Eq, Ord, Read, Show)
