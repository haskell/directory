import System.Directory
import Control.Exception

testdir = "createDirectory001.dir"

main = do
  try (removeDirectory testdir) :: IO (Either IOException ())
  createDirectory testdir
  r <- try $ createDirectory testdir
  print (r :: Either IOException ()) -- already exists
  removeDirectory testdir

