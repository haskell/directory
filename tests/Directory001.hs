module Directory001 where
import Prelude ()
import System.Directory.Internal.Prelude
import System.Directory.OsPath
import TestUtils ()
import Util (TestEnv)
import qualified Util as T

main :: TestEnv -> IO ()
main _t = do

  createDirectory "foo"
  writeFile "foo/bar" str
  renameFile "foo/bar" "foo/baz"
  renameDirectory "foo" "bar"
  str' <- readFile "bar/baz"
  T.expectEq _t () str' str
  removeFile "bar/baz"
  removeDirectory "bar"

  where
    str = "Okay\n"
