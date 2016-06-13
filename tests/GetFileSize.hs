{-# LANGUAGE CPP #-}
module GetFileSize where
#include "util.inl"
import System.Directory
import qualified System.IO as IO

main :: TestEnv -> IO ()
main _t = do

  IO.withBinaryFile "emptyfile" IO.WriteMode $ \ _ -> do
    return ()
  IO.withBinaryFile "testfile" IO.WriteMode $ \ h -> do
    IO.hPutStr h string

  T(expectEq) () 0 =<< getFileSize "emptyfile"
  T(expectEq) () (fromIntegral (length string)) =<< getFileSize "testfile"

  where
    string = "The quick brown fox jumps over the lazy dog."
