module CopyFile002 where
import Prelude ()
import System.Directory.Internal.Prelude
import System.Directory.Internal
import System.Directory.OsPath
import TestUtils ()
import Util (TestEnv)
import qualified Util as T
import qualified Data.List as List

main :: TestEnv -> IO ()
main _t = do
  -- Similar to CopyFile001 but moves a file in the current directory
  -- (Bug #1652 on GHC Trac)
  writeFile (so from) contents
  T.expectEq _t () [from] . List.sort =<< listDirectory "."
  copyFile from to
  T.expectEq _t () [from, to] . List.sort =<< listDirectory "."
  T.expectEq _t () contents =<< readFile (so to)
  where
    contents = "This is the data\n"
    from     = "source"
    to       = "target"
