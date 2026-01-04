module GetFileSize where
import Prelude ()
import System.Directory.Internal.Prelude
import System.Directory.OsPath
import TestUtils ()
import Util (TestEnv)
import qualified Util as T

main :: TestEnv -> IO ()
main _t = do

  writeFile "emptyfile" ""
  writeFile "testfile" string

  T.expectEq _t () 0 =<< getFileSize "emptyfile"
  T.expectEq _t () (fromIntegral (length string)) =<< getFileSize "testfile"

  where
    string = "The quick brown fox jumps over the lazy dog."
