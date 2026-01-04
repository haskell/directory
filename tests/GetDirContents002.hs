module GetDirContents002 where
import Prelude ()
import System.Directory.Internal.Prelude
import System.Directory.OsPath
import TestUtils ()
import Util (TestEnv)
import qualified Util as T

main :: TestEnv -> IO ()
main _t = do
  T.expectIOErrorType _t () isDoesNotExistError $
    getDirectoryContents "nonexistent"
