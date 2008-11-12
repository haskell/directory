import System.Directory
import Control.Exception
import System.FilePath
import Data.List

dir = "getDirContents001.dir"

main = do
    try cleanup :: IO (Either IOException ())
    bracket (createDirectory dir) (const cleanup) $ \_ -> do
      getDirectoryContents dir >>= print . sort
      mapM_ (\s -> writeFile (dir </> ('f':show s)) (show s)) [1..100]
      getDirectoryContents dir >>= print . sort

cleanup = do
   files <- getDirectoryContents dir
   mapM_ (removeFile . (dir </>)) (filter (not . ("." `isPrefixOf`)) files)
   removeDirectory dir
