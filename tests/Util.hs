{-# LANGUAGE BangPatterns #-}
-- | A rudimentary testing framework
module Util where
import Prelude ()
import System.Directory.Internal.Prelude
import System.Directory.Internal
import System.Directory.OsPath
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime)
import GHC.Stack (CallStack, HasCallStack, callStack, getCallStack, srcLocFile,
                  srcLocStartLine)
import System.Environment (getEnvironment, setEnv, unsetEnv)
import System.OsPath ((</>), decodeFS, encodeFS, normalise)
import qualified Data.List as List

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
    Nothing -> throwIO (userError "timed out")
    Just x  -> return x

data TestEnv =
  TestEnv
  { testCounter  :: IORef Int
  , testSilent   :: Bool
  , testKeepDirs :: Bool
  , testArgs     :: [(String, String)]
  }

printInfo :: TestEnv -> [String] -> IO ()
printInfo TestEnv{testSilent = True}  _   = return ()
printInfo TestEnv{testSilent = False} msg = do
  putStrLn (List.intercalate ": " msg)
  hFlush stdout

printErr :: [String] -> IO ()
printErr msg = do
  hPutStrLn stderr ("*** " <> List.intercalate ": " msg)
  hFlush stderr

printFailure :: TestEnv -> [String] -> IO ()
printFailure TestEnv{testCounter = n} msg = do
  modifyIORef' n (+ 1)
  printErr msg

check :: TestEnv -> Bool -> [String] -> [String] -> [String] -> IO ()
check t True  prefix msg _   = printInfo t (prefix <> msg)
check t False prefix _   msg = printFailure t (prefix <> msg)

checkEither :: TestEnv -> [String] -> Either [String] [String] -> IO ()
checkEither t prefix (Right msg) = printInfo t (prefix <> msg)
checkEither t prefix (Left  msg) = printFailure t (prefix <> msg)

showContext :: Show a => CallStack -> a -> String
showContext stack context =
  case getCallStack stack of
    (_, l) : _ -> srcLocFile l <> ":" <> show (srcLocStartLine l)
    [] -> "<unknown caller>"
  <>
  case show context of
    "()" -> ""
    s    -> ":" <> s

inform :: HasCallStack => TestEnv -> String -> IO ()
inform t msg =
  printInfo t [showContext callStack (), msg]


expect :: (HasCallStack, Show a) => TestEnv -> a -> Bool -> IO ()
expect t context x =
  check t x
  [showContext callStack context]
  ["True"]
  ["False, but True was expected"]

expectEq :: (HasCallStack, Eq a, Show a, Show b) =>
            TestEnv -> b -> a -> a -> IO ()
expectEq t context x y =
  check t (x == y)
  [showContext callStack context]
  [show x <> " equals "     <> show y]
  [show x <> " is not equal to " <> show y]

expectNe :: (HasCallStack, Eq a, Show a, Show b) =>
            TestEnv -> b -> a -> a -> IO ()
expectNe t context x y =
  check t (x /= y)
  [showContext callStack context]
  [show x <> " is not equal to " <> show y]
  [show x <> " equals "     <> show y]

expectNear :: (HasCallStack, Num a, Ord a, Show a, Show b) =>
              TestEnv -> b -> a -> a -> a -> IO ()
expectNear t context x y diff =
  check t (abs (x - y) <= diff)
  [showContext callStack context]
  [show x <> " is near "     <> show y]
  [show x <> " is not near " <> show y]

expectNearTime :: (HasCallStack, Show a) =>
                  TestEnv -> a ->
                  UTCTime -> UTCTime -> NominalDiffTime -> IO ()
expectNearTime t context x y diff =
  check t (abs (diffUTCTime x y) <= diff)
  [showContext callStack context]
  [show x <> " is near "     <> show y]
  [show x <> " is not near " <> show y]

expectIOErrorType :: (HasCallStack, Show a) =>
                     TestEnv -> a -> (IOError -> Bool) -> IO b -> IO ()
expectIOErrorType t context which action = do
  result <- try action
  checkEither t [showContext callStack context] $ case result of
    Left  e | which e   -> Right ["got expected exception (" <> show e <> ")"]
            | otherwise -> Left  ["got wrong exception: ", show e]
    Right _             -> Left  ["did not throw an exception"]

-- | Traverse the directory tree in preorder.
preprocessPathRecursive :: (OsPath -> IO ()) -> OsPath -> IO ()
preprocessPathRecursive f path = do
  dirExists <- doesDirectoryExist path
  if dirExists
    then do
      isLink <- pathIsSymbolicLink path
      f path
      when (not isLink) $ do
        names <- listDirectory path
        for_ ((path </>) <$> names) (preprocessPathRecursive f)
    else do
      f path

withNewDirectory :: Bool -> OsPath -> IO a -> IO a
withNewDirectory keep dir action = do
  dir' <- makeAbsolute dir
  bracket_ (createDirectoryIfMissing True dir') (cleanup dir') action
  where cleanup dir' | keep      = return ()
                     | otherwise = removePathForcibly dir'

diffAsc' :: (j -> k -> Ordering)
         -> (u -> v -> Bool)
         -> [(j, u)]
         -> [(k, v)]
         -> ([(j, u)], [(k, v)])
diffAsc' cmp eq = go id id
  where
    go a b [] [] = (a [], b [])
    go a b jus [] = go (a . (jus <>)) b [] []
    go a b [] kvs = go a (b . (kvs <>)) [] []
    go a b jus@((j, u) : jus') kvs@((k, v) : kvs') =
      case cmp j k of
        LT -> go (a . ((j, u) :)) b jus' kvs
        GT -> go a (b . ((k, v) :)) jus kvs'
        EQ | eq u v -> go a b jus' kvs'
           | otherwise -> go (a . ((j, u) :)) (b . ((k, v) :)) jus' kvs'

diffAsc :: (Ord k, Eq v) => [(k, v)] -> [(k, v)] -> ([(k, v)], [(k, v)])
diffAsc = diffAsc' compare (==)

-- Environment variables may be sensitive, so don't log their values.
scrubEnv :: (String, String) -> (String, String)
scrubEnv (k, v)
  -- Allowlist for nonsensitive variables.
  | k `elem` ["XDG_CONFIG_HOME"] = (k, v)
  | otherwise = (k, "<" <> show (length v) <> " chars>")

isolateEnvironment :: IO a -> IO a
isolateEnvironment = bracket getEnvs setEnvs . const
  where
    -- Duplicate environment variables will cause problems for this code.
    -- https://github.com/haskell/cabal/issues/10718
    getEnvs = List.sort . filter (\(k, _) -> k /= "") <$> getEnvironment
    setEnvs target = do
      current <- getEnvs
      let (deletions, insertions) = diffAsc current target
      updateEnvs deletions insertions
      new <- getEnvs
      when (target /= new) $ do
        let (missing, extraneous) = diffAsc target new
        throwIO (userError ("isolateEnvironment.setEnvs failed:" <>
                            " deletions=" <> show (scrubEnv <$> deletions) <>
                            " insertions=" <> show (scrubEnv <$> insertions) <>
                            " missing=" <> show (scrubEnv <$> missing) <>
                            " extraneous=" <> show (scrubEnv <$> extraneous)))
    updateEnvs deletions insertions = do
      for_ deletions (unsetEnv . fst)
      for_ insertions (uncurry setEnv)

isolateWorkingDirectory :: Bool -> OsPath -> IO a -> IO a
isolateWorkingDirectory keep dir action = do
  normalisedDir <- decodeFS (normalise dir)
  when (normalisedDir `List.elem` [".", "./"]) $
    throwIO (userError ("isolateWorkingDirectory cannot be used " <>
                        "with current directory"))
  dir' <- makeAbsolute dir
  removePathForcibly dir'
  withNewDirectory keep dir' $
    withCurrentDirectory dir' $
      action

run :: TestEnv -> String -> (TestEnv -> IO ()) -> IO ()
run t name action = do
  result <- tryAny (action t)
  case result of
    Left  e  -> check t False [name] [] ["exception", show e]
    Right () -> return ()

isolatedRun :: TestEnv -> String -> (TestEnv -> IO ()) -> IO ()
isolatedRun t@TestEnv{testKeepDirs = keep} name action = do
  workDir <- encodeFS ("dist/test-" <> name <> ".tmp")
  run t name (isolate workDir . action)
  where
    isolate workDir = isolateEnvironment . isolateWorkingDirectory keep workDir

tryRead :: Read a => String -> Maybe a
tryRead s =
  case reads s of
    [(x, "")] -> Just x
    _         -> Nothing

getArg :: (String -> Maybe a) -> TestEnv -> String -> String -> a -> a
getArg parse TestEnv{testArgs = args} testname name defaultValue =
  fromMaybe defaultValue (List.lookup (prefix <> name) args >>= parse)
  where prefix | testname == "" = ""
               | otherwise      = testname <> "."

readArg :: Read a => TestEnv -> String -> String -> a -> a
readArg = getArg tryRead

readBool :: String -> Maybe Bool
readBool s = Just $
  case toLower <$> s of
    'y' : _ -> True
    't' : _ -> True
    _       -> False

parseArgs :: [String] -> [(String, String)]
parseArgs = List.reverse . (second (List.drop 1) . List.span (/= '=') <$>)

testMain :: (TestEnv -> IO ()) -> IO ()
testMain action = do
  args <- parseArgs <$> getArgs
  counter <- newIORef 0
  let t = TestEnv
          { testCounter  = counter
          , testSilent   = getArg readBool t "" "silent" False
          , testKeepDirs = getArg readBool t "" "keep-dirs" False
          , testArgs     = args
          }
  action t
  n <- readIORef (counter)
  unless (n == 0) $ do
    putStrLn ("[" <> show n <> " failures]")
    hFlush stdout
    exitFailure
