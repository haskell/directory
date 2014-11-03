import System.Directory
import Control.Exception

tmp1 = "T8482.tmp1"
testdir = "T8482.dir"

main = do
  writeFile tmp1 "hello"
  createDirectory testdir
  tryRenameFile testdir tmp1 >>= print  -- InappropriateType
  tryRenameFile tmp1 testdir >>= print  -- InappropriateType
  tryRenameFile tmp1 "." >>= print  -- InappropriateType
  removeDirectory testdir
  removeFile tmp1
  where tryRenameFile :: FilePath -> FilePath -> IO (Either IOException ())
        tryRenameFile opath npath = try $ renameFile opath npath
