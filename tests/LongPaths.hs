module LongPaths where
import Prelude ()
import System.Directory.Internal.Prelude
import System.Directory.OsPath
import TestUtils
import Util (TestEnv)
import qualified Util as T
import System.OsPath ((</>))

main :: TestEnv -> IO ()
main _t = do
  let longName = mconcat (replicate 10 "its_very_long")
  longDir <- makeAbsolute (longName </> longName)

  supportsLongPaths <- do
      -- create 2 dirs because 1 path segment by itself can't exceed MAX_PATH
      -- tests: [createDirectory]
      createDirectory =<< makeAbsolute longName
      createDirectory longDir
      pure True
    `catchIOError` \ _ ->
      pure False

  -- skip tests on file systems that do not support long paths
  when supportsLongPaths $ do

    -- test relative paths
    let relDir = longName </> mconcat (replicate 8 "yeah_its_long")
    createDirectory relDir
    T.expect _t () =<< doesDirectoryExist relDir
    T.expectEq _t () [] =<< listDirectory relDir
    setPermissions relDir emptyPermissions
    T.expectEq _t () False =<< writable <$> getPermissions relDir

    writeFile "foobar.txt" "^.^" -- writeFile does not support long paths yet

    -- tests: [renamePath], [copyFileWithMetadata]
    renamePath "foobar.txt" (longDir </> "foobar_tmp.txt")
    renamePath (longDir </> "foobar_tmp.txt") (longDir </> "foobar.txt")
    copyFileWithMetadata (longDir </> "foobar.txt")
                         (longDir </> "foobar_copy.txt")

    -- tests: [doesDirectoryExist], [doesFileExist], [doesPathExist]
    T.expect _t () =<< doesDirectoryExist longDir
    T.expect _t () =<< doesFileExist (longDir </> "foobar.txt")
    T.expect _t () =<< doesPathExist longDir
    T.expect _t () =<< doesPathExist (longDir </> "foobar.txt")

    -- tests: [getFileSize], [getModificationTime]
    T.expectEq _t () 3 =<< getFileSize (longDir </> "foobar.txt")
    _ <- getModificationTime (longDir </> "foobar.txt")

    supportsSymbolicLinks <- supportsSymlinks
    when supportsSymbolicLinks $ do

      -- tests: [createDirectoryLink], [getSymbolicLinkTarget], [listDirectory]
      -- also tests expansion of "." and ".."
      createDirectoryLink "." (longDir </> "link")
      _ <- listDirectory (longDir </> ".." </> longName </> "link")
      T.expectEq _t () "." =<<
        getSymbolicLinkTarget (longDir </> "." </> "link")

      pure ()

  -- [removeFile], [removeDirectory] are automatically tested by the cleanup
