module WithCurrentDirectory where
import Prelude ()
import System.Directory.Internal.Prelude
import System.Directory.Internal
import System.Directory.OsPath
import TestUtils ()
import Util (TestEnv)
import qualified Util as T
import System.OsPath ((</>))
import qualified Data.List as List

main :: TestEnv -> IO ()
main _t = do
  createDirectory dir
  -- Make sure we're starting empty
  T.expectEq _t () [] . List.sort =<< listDirectory dir
  cwd <- getCurrentDirectory
  withCurrentDirectory dir (writeFile (so testfile) contents)
  -- Are we still in original directory?
  T.expectEq _t () cwd =<< getCurrentDirectory
  -- Did the test file get created?
  T.expectEq _t () [testfile] . List.sort =<< listDirectory dir
  -- Does the file contain what we expected to write?
  T.expectEq _t () contents =<< readFile (so (dir </> testfile))
  where
    testfile = "testfile"
    contents = "some data\n"
    dir = "dir"
