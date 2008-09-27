
module Main (main) where

import Control.Exception
import Data.List
import System.Directory
import System.IO

main :: IO ()
main = do tryIO $ removeFile to
          cs_before <- getDirectoryContents "copyFile"
          putStrLn "Before:"
          print $ sort cs_before
          copyFile from to
          cs_before <- getDirectoryContents "copyFile"
          putStrLn "After:"
          print $ sort cs_before
          readFile to >>= print

tryIO :: IO a -> IO (Either IOException a)
tryIO = try 

from, to :: FilePath
from = "copyFile/source"
to   = "copyFile/target"

