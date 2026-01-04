module CopyFile001 where
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
  writeFile (so (dir </> from)) contents
  T.expectEq _t () [from] . List.sort =<< listDirectory dir
  copyFile (dir </> from) (dir </> to)
  T.expectEq _t () [from, to] . List.sort =<< listDirectory dir
  T.expectEq _t () contents =<< readFile (so (dir </> to))

  -- Regression test for https://github.com/haskell/directory/issues/177
  createDirectory "issue177"
  T.expectIOErrorType _t () isDoesNotExistError
    (copyFile "issue177/nonexistentSrc" "issue177/dst")
  T.expectEq _t () [] =<< listDirectory "issue177"

  where
    contents = "This is the data\n"
    from     = "source"
    to       = "target"
    dir      = "dir"
