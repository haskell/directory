module PathIsSymbolicLink where
import Prelude ()
import System.Directory.Internal.Prelude
import System.Directory.OsPath
import TestUtils
import Util (TestEnv)
import qualified Util as T

main :: TestEnv -> IO ()
main _t = do
  supportsSymbolicLinks <- supportsSymlinks
  when supportsSymbolicLinks $ do

    createFileLink "x" "y"
    createDirectoryLink "a" "b"

    T.expect _t () =<< pathIsSymbolicLink "y"
    T.expect _t () =<< pathIsSymbolicLink "b"
    T.expectEq _t () "x" =<< getSymbolicLinkTarget "y"
    T.expectEq _t () "a" =<< getSymbolicLinkTarget "b"
    T.expectEq _t () False =<< doesFileExist "y"
    T.expectEq _t () False =<< doesDirectoryExist "b"

    writeFile "x" ""
    createDirectory "a"

    T.expect _t () =<< doesFileExist "y"
    T.expect _t () =<< doesDirectoryExist "b"

    removeFile "y"
    removeDirectoryLink "b"

    T.expectIOErrorType _t () isDoesNotExistError (pathIsSymbolicLink "y")
    T.expectIOErrorType _t () isDoesNotExistError (pathIsSymbolicLink "b")
    T.expectEq _t () False =<< doesFileExist "y"
    T.expectEq _t () False =<< doesDirectoryExist "b"
