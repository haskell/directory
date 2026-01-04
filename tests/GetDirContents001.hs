module GetDirContents001 where
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
  T.expectEq _t () specials . List.sort =<<
    getDirectoryContents dir
  T.expectEq _t () [] . List.sort =<<
    listDirectory dir
  names <- for [1 .. 100 :: Int] $ \ i -> do
    let name = "f" <> os (show i)
    writeFile (so (dir </> name)) ""
    return name
  T.expectEq _t () (List.sort (specials <> names)) . List.sort =<<
    getDirectoryContents dir
  T.expectEq _t () (List.sort names) . List.sort =<<
    listDirectory dir
  where dir      = "dir"
        specials = [".", ".."]
