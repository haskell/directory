module Main(main) where

import Control.Exception
import System.Directory

main = do
  dot <- canonicalizePath "."
  nul <- (canonicalizePath "")
           `catch` ((\_ -> return "") :: IOException -> IO String)
  print (dot == nul)
