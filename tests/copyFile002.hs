
module Main (main) where

import Control.Exception
import Data.List
import System.Directory
import System.IO

-- like copyFile001, but moves a file in the current directory
-- See bug #1652
main :: IO ()
main = do d <- getCurrentDirectory
          flip finally (setCurrentDirectory d) $ do
          setCurrentDirectory "copyFile002dir"
          tryIO $ removeFile to
          cs_before <- getDirectoryContents "."
          putStrLn "Before:"
          print $ sort cs_before
          copyFile from to
          cs_before <- getDirectoryContents "."
          putStrLn "After:"
          print $ sort cs_before
          readFile to >>= print

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

from, to :: FilePath
from = "source"
to   = "target"

