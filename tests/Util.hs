{-# LANGUAGE BangPatterns, CPP #-}
module Util where
import Prelude (Eq(..), Num(..), Ord(..), RealFrac(..), Show(..),
                Bool(..), Double, Either(..), Int, Integer, Maybe(..), String,
                ($), (.), otherwise)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (elem, intercalate)
import Data.Monoid ((<>))
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime)
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Exception (SomeException, bracket_, catch,
                          mask, onException, try)
import Control.Monad (Monad(..), unless, when)
import System.Directory (createDirectory, getCurrentDirectory, makeAbsolute,
                         removeDirectoryRecursive, setCurrentDirectory)
import System.Exit (exitFailure)
import System.FilePath (FilePath, (</>), normalise)
import System.IO (IO, hFlush, hPutStrLn, putStrLn, stderr, stdout)
import System.IO.Error (IOError, isDoesNotExistError,
                        ioError, tryIOError, userError)
import System.Timeout (timeout)

modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' r f = do
  x <- readIORef r
  let !x' = f x in writeIORef r x'

tryAny :: IO a -> IO (Either SomeException a)
tryAny action = do
  result <- newEmptyMVar
  mask $ \ unmask -> do
    thread <- forkIO (try (unmask action) >>= putMVar result)
    unmask (readMVar result) `onException` killThread thread

timeLimit :: Double -> IO a -> IO a
timeLimit time action = do
  result <- timeout (round (1000000 * time)) action
  case result of
    Nothing -> ioError (userError "timed out")
    Just x  -> return x

data TestEnv =
  TestEnv
  { testCounter  :: IORef Int
  , testSilent   :: Bool
  , testKeepDirs :: Bool
  }

defaultTestEnv :: IORef Int -> TestEnv
defaultTestEnv counter =
  TestEnv
  { testCounter  = counter
  , testSilent   = False
  , testKeepDirs = False
  }

showSuccess :: TestEnv -> [String] -> IO ()
showSuccess TestEnv{testSilent = True}  _   = return ()
showSuccess TestEnv{testSilent = False} msg = do
  putStrLn (intercalate ": " msg)
  hFlush stdout

showFailure :: TestEnv -> [String] -> IO ()
showFailure TestEnv{testCounter = n} msg = do
  modifyIORef' n (+ 1)
  hPutStrLn stderr ("*** " <> intercalate ": " msg)
  hFlush stderr

check :: TestEnv -> Bool -> [String] -> [String] -> [String] -> IO ()
check t True  prefix msg _   = showSuccess t (prefix <> msg)
check t False prefix _   msg = showFailure t (prefix <> msg)

checkEither :: TestEnv -> [String] -> Either [String] [String] -> IO ()
checkEither t prefix (Right msg) = showSuccess t (prefix <> msg)
checkEither t prefix (Left  msg) = showFailure t (prefix <> msg)

showContext :: Show a => String -> Integer -> a -> String
showContext file line context =
  file <> ":" <> show line <>
  case show context of
    "()" -> ""
    s    -> ":" <> s

expect :: Show a => TestEnv -> String -> Integer -> a -> Bool -> IO ()
expect t file line context x =
  check t x
  [showContext file line context]
  ["True"]
  ["False, but True was expected"]

expectNear :: (Num a, Ord a, Show a, Show b) =>
              TestEnv -> String -> Integer -> b -> a -> a -> a -> IO ()
expectNear t file line context x y diff =
  check t (abs (x - y) <= diff)
  [showContext file line context]
  [show x <> " is near "     <> show y]
  [show x <> " is not near " <> show y]

expectNearTime :: Show a =>
                  TestEnv -> String -> Integer -> a ->
                  UTCTime -> UTCTime -> NominalDiffTime -> IO ()
expectNearTime t file line context x y diff =
  check t (abs (diffUTCTime x y) <= diff)
  [showContext file line context]
  [show x <> " is near "     <> show y]
  [show x <> " is not near " <> show y]

expectIOErrorType :: Show a =>
                     TestEnv -> String -> Integer -> a
                  -> (IOError -> Bool) -> IO b -> IO ()
expectIOErrorType t file line context which action = do
  result <- tryIOError action
  checkEither t [showContext file line context] $ case result of
    Left  e | which e   -> Right ["got expected exception (" <> show e <> ")"]
            | otherwise -> Left  ["got wrong exception: ", show e]
    Right _             -> Left  ["did not throw an exception"]

withWorkingDirectory :: FilePath -> IO a -> IO a
withWorkingDirectory dir action = do
  cur  <- getCurrentDirectory
  bracket_ (setCurrentDirectory (cur </> dir))
           (setCurrentDirectory  cur) action

withNewDirectory :: FilePath -> IO a -> IO a
withNewDirectory dir action = do
  dir' <- makeAbsolute dir
  bracket_ (createDirectory          dir')
           (removeDirectoryRecursive dir') action

isolateWorkingDirectory :: FilePath -> IO a -> IO a
isolateWorkingDirectory dir action = do
  when (normalise dir `elem` [".", "./"]) $
    ioError (userError ("isolateWorkingDirectory cannot be used " <>
                        "with current directory"))
  dir' <- makeAbsolute dir
  removeDirectoryRecursive dir' `catch` \ e ->
    unless (isDoesNotExistError e) $
      ioError e
  withNewDirectory dir' $
    withWorkingDirectory dir' $
      action

run :: TestEnv -> String -> (TestEnv -> IO ()) -> IO ()
run t name action = do
  result <- tryAny (action t)
  case result of
    Left  e  -> check t False [name] [] ["exception", show e]
    Right () -> return ()

isolatedRun :: TestEnv -> String -> (TestEnv -> IO ()) -> IO ()
isolatedRun t name action = do
  run t name (isolateWorkingDirectory ("test-" <> name <> ".tmp") . action)

testMain :: (TestEnv -> IO ()) -> IO ()
testMain action = do
  counter <- newIORef 0
  action (defaultTestEnv counter)
  n <- readIORef (counter)
  unless (n == 0) $ do
    putStrLn ("[" <> show n <> " failures]")
    hFlush stdout
    exitFailure
