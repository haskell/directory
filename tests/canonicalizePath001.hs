module Main(main) where

import Control.Concurrent
import Control.Monad
import Control.Exception
import System.Directory
import System.FilePath
import System.IO.Error

main = do
  dot <- canonicalizePath "."
  nul <- (canonicalizePath "")
           `catch` ((\_ -> return "") :: IOException -> IO String)
  print $ dot == nul

report :: Show a => IO a -> IO ()
report io = do
  r <- try io
  case r of
   Left e  -> print (e :: SomeException)
   Right a -> print a

ignore :: IO a -> IO ()
ignore io = do
  r <- try io
  case r of
   Left e  -> let _ = e :: SomeException in return ()
   Right a -> return ()
