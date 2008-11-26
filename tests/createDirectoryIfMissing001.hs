module Main(main) where

import Control.Concurrent
import Control.Monad
import Control.Exception
import System.Directory
import System.FilePath

testdir = "createDirectory001"
testdir_a = testdir </> "a"

main = do
  cleanup

  report $ createDirectoryIfMissing False testdir
  cleanup

  report $ createDirectoryIfMissing False testdir_a
   -- should fail with does not exist

  report $ createDirectoryIfMissing True testdir_a
   -- should succeed with no error
  report $ createDirectoryIfMissing False testdir_a
   -- should succeed with no error
  report $ createDirectoryIfMissing False (addTrailingPathSeparator testdir_a)
   -- should succeed with no error

  cleanup
  report $ createDirectoryIfMissing True (addTrailingPathSeparator testdir_a)

  -- look for race conditions: #2808.  This fails with
  -- +RTS -N2 and directory 1.0.0.2.
  m <- newEmptyMVar
  forkIO $ do replicateM_ 10000 create; putMVar m ()
  forkIO $ do replicateM_ 10000 cleanup; putMVar m ()
  replicateM_ 2 $ takeMVar m
  cleanup

create = createDirectoryIfMissing True testdir_a

cleanup = ignore $ removeDirectoryRecursive testdir

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
