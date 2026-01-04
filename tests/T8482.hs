module T8482 where
import Prelude ()
import System.Directory.Internal.Prelude
import System.Directory.Internal
import System.Directory.OsPath
import TestUtils ()
import Util (TestEnv)
import qualified Util as T

tmp1 :: OsPath
tmp1 = "T8482.tmp1"

testdir :: OsPath
testdir = "T8482.dir"

main :: TestEnv -> IO ()
main _t = do
  writeFile (so tmp1) "hello"
  createDirectory testdir
  T.expectIOErrorType _t () (is InappropriateType) (renameFile testdir tmp1)
  T.expectIOErrorType _t () (is InappropriateType) (renameFile tmp1    testdir)
  T.expectIOErrorType _t () (is InappropriateType) (renameFile tmp1    ".")
  where is t = (== t) . ioeGetErrorType
