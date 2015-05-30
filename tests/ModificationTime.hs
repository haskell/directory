{-# LANGUAGE CPP #-}
module ModificationTime where
#include "util.inl"
import System.Directory
import Data.Foldable (for_)
import qualified Data.Time.Clock as Time

main :: TestEnv -> IO ()
main _t = do
  now <- Time.getCurrentTime
  let someTimeAgo = Time.addUTCTime (-3600) now

  writeFile  "foo" ""
  for_ [ ("foo", someTimeAgo)
       , (".",   someTimeAgo)
       , ("",    someTimeAgo) ] $ \ (file, mtime1) -> do

    setModificationTime file mtime1
    mtime2 <- getModificationTime file

    -- modification time should be set with at worst 1 sec resolution
    T(expectNearTime) ("mtime", file) mtime1 mtime2 1
