module CurrentDirectory001 where
import Prelude ()
import System.Directory.Internal.Prelude
import System.Directory.OsPath
import TestUtils ()
import Util (TestEnv)
import qualified Util as T
import qualified Data.List as List

main :: TestEnv -> IO ()
main _t = do
  prevDir <- getCurrentDirectory
  createDirectory "dir"
  setCurrentDirectory "dir"
  T.expectEq _t () [".", ".."] . List.sort =<< getDirectoryContents "."
  setCurrentDirectory prevDir
  removeDirectory "dir"
