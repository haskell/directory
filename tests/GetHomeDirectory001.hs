module GetHomeDirectory001 where
import Prelude ()
import System.Directory.Internal.Prelude
import System.Directory.OsPath
import TestUtils ()
import Util (TestEnv)
import qualified Util as T

main :: TestEnv -> IO ()
main _t = do
  homeDir <- getHomeDirectory
  T.expect _t () (homeDir /= mempty) -- sanity check
  _ <- getAppUserDataDirectory   "test"
  _ <- getXdgDirectory XdgCache  "test"
  _ <- getXdgDirectory XdgConfig "test"
  _ <- getXdgDirectory XdgData   "test"
  _ <- getXdgDirectory XdgState  "test"
  _ <- getUserDocumentsDirectory
  _ <- getTemporaryDirectory
  pure ()
