{-# LANGUAGE CPP #-}
module CreateDirectoryIfMissing001 where
import Prelude ()
import System.Directory.Internal.Prelude
import System.Directory.Internal
import System.Directory.OsPath
import TestUtils ()
import Util (TestEnv)
import qualified Util as T
import Data.Either (lefts)
import System.OsPath ((</>), addTrailingPathSeparator)

main :: TestEnv -> IO ()
main _t = do

  createDirectoryIfMissing False testdir
  cleanup

  T.expectIOErrorType _t () isDoesNotExistError $
    createDirectoryIfMissing False testdir_a

  createDirectoryIfMissing True  testdir_a
  createDirectoryIfMissing False testdir_a
  createDirectoryIfMissing False (addTrailingPathSeparator testdir_a)
  cleanup

  createDirectoryIfMissing True  (addTrailingPathSeparator testdir_a)

  T.inform _t "testing for race conditions ..."
  raceCheck1
  T.inform _t "testing for race conditions ..."
  raceCheck2
  T.inform _t "done."
  cleanup

  writeFile (so testdir) (so testdir)
  T.expectIOErrorType _t () isAlreadyExistsError $
    createDirectoryIfMissing False testdir
  removeFile testdir
  cleanup

  writeFile (so testdir) (so testdir)
  T.expectIOErrorType _t () isNotADirectoryError $
    createDirectoryIfMissing True testdir_a
  removeFile testdir
  cleanup

  where

    testname = "CreateDirectoryIfMissing001"

    testdir = os (testname <> ".d")
    testdir_a = testdir </> "a"

    numRepeats = T.readArg _t testname "num-repeats" 10
    numThreads = T.readArg _t testname "num-threads" 4

    forkPut mvar action = () <$ forkFinally action (putMVar mvar)

    -- Look for race conditions (bug #2808 on GHC Trac).  This fails with
    -- +RTS -N2 and directory 1.0.0.2.
    raceCheck1 = do
      m <- newEmptyMVar
      forkPut m $ do
        replicateM_ numRepeats create
      forkPut m $ do
        replicateM_ numRepeats cleanup
      results <- replicateM 2 (takeMVar m)
      T.expectEq _t () [] (show <$> lefts results)

    -- This test fails on Windows (see bug #2924 on GHC Trac):
    raceCheck2 = do
      m <- newEmptyMVar
      replicateM_ numThreads $
        forkPut m $ do
          replicateM_ numRepeats $ do
            create
            cleanup
      results <- replicateM numThreads (takeMVar m)
      T.expectEq _t () [] (show <$> lefts results)

    -- createDirectoryIfMissing is allowed to fail with isDoesNotExistError if
    -- another process/thread removes one of the directories during the process
    -- of creating the hierarchy.
    --
    -- It is also allowed to fail with permission errors
    -- (see bug #2924 on GHC Trac)
    create =
      createDirectoryIfMissing True testdir_a `catch` \ e ->
      if isDoesNotExistError e
         || isPermissionError e
         || isInappropriateTypeError e
         || ioeGetErrorType e == InvalidArgument
      then return ()
      else ioError e

    cleanup = removeDirectoryRecursive testdir `catchAny` \ _ -> return ()

    catchAny :: IO a -> (SomeException -> IO a) -> IO a
    catchAny = catch

#if defined(mingw32_HOST_OS)
    isNotADirectoryError = isAlreadyExistsError
#else
    isNotADirectoryError = isInappropriateTypeError
#endif

    isInappropriateTypeError = isInappropriateTypeErrorType . ioeGetErrorType

    isInappropriateTypeErrorType InappropriateType = True
    isInappropriateTypeErrorType _                 = False
