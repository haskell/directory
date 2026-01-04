module RenameDirectory where
import Prelude ()
import System.Directory.Internal.Prelude
import System.Directory.OsPath
import TestUtils ()
import Util (TestEnv)
import qualified Util as T

main :: TestEnv -> IO ()
main _t = do
  createDirectory "a"
  T.expectEq _t () ["a"] =<< listDirectory "."
  renameDirectory "a" "b"
  T.expectEq _t () ["b"] =<< listDirectory "."
