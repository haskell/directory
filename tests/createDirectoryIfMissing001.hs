module Main(main) where

import Control.Concurrent
import Control.Monad
import Control.Exception
import System.Directory
import System.FilePath
import System.IO.Error

testdir = "createDirectoryIfMissing001.d"
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

-- This test fails on Windows; see #2924
--  replicateM_ 2 $ 
--     forkIO $ do replicateM_ 5000 (do create; cleanup); putMVar m ()
--  replicateM_ 2 $ takeMVar m

  cleanup

  -- these are all supposed to fail

  writeFile testdir testdir
  report $ createDirectoryIfMissing False testdir
  removeFile testdir
  cleanup

  writeFile testdir testdir
  report $ createDirectoryIfMissing True testdir_a
  removeFile testdir
  cleanup

-- createDirectoryIfMissing is allowed to fail with isDoesNotExistError if
-- another process/thread removes one of the directories during the proces
-- of creating the hierarchy.
--
-- It is also allowed to fail with permission errors (see #2924)
create = tryJust (guard . (\e -> isDoesNotExistError e || isPermissionError e)) $ createDirectoryIfMissing True testdir_a

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
