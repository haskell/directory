{-# LANGUAGE CPP #-}
module FileTime where
#include "util.inl"
import System.Directory
import System.IO.Error (isDoesNotExistError)
import Data.Foldable (for_)
import qualified Data.Time.Clock as Time

main :: TestEnv -> IO ()
main _t = do
  now <- Time.getCurrentTime
  let someTimeAgo = Time.addUTCTime (-3600) now

  T(expectIOErrorType) () isDoesNotExistError $
    getAccessTime "nonexistent-file"
  T(expectIOErrorType) () isDoesNotExistError $
    getModificationTime "nonexistent-file"
  T(expectIOErrorType) () isDoesNotExistError $
    setModificationTime "nonexistent-file" someTimeAgo

  writeFile  "foo" ""
  for_ [ ("foo", someTimeAgo)
       , (".",   someTimeAgo)
       , ("",    someTimeAgo) ] $ \ (file, mtime1) -> do

    atime1 <- getAccessTime file
    setModificationTime file mtime1
    atime2 <- getAccessTime file
    mtime2 <- getModificationTime file

    -- modification time should be set with at worst 1 sec resolution
    T(expectNearTime) ("mtime", file) mtime1 mtime2 1

    -- access time should not change, although it may lose some precision
    -- on POSIX systems without 'utimensat'
    T(expectNearTime) ("atime", file) atime1 atime2 1
