module GetPermissions001 where
import Prelude ()
import System.Directory.Internal.Prelude
import System.Directory.OsPath
import TestUtils
import Util (TestEnv)
import qualified Util as T

main :: TestEnv -> IO ()
main _t = do

  checkCurrentDir
  checkExecutable
  checkOrdinary
  checkTrailingSlash

  -- 'writable' is the only permission that can be changed on Windows
  writeFile "foo.txt" ""
  foo <- makeAbsolute "foo.txt"
  modifyPermissions "foo.txt" (\ p -> p { writable = False })
  T.expect _t () =<< not . writable <$> getPermissions "foo.txt"
  modifyPermissions "foo.txt" (\ p -> p { writable = True })
  T.expect _t () =<< writable <$> getPermissions "foo.txt"
  modifyPermissions "foo.txt" (\ p -> p { writable = False })
  T.expect _t () =<< not . writable <$> getPermissions "foo.txt"
  modifyPermissions foo (\ p -> p { writable = True })
  T.expect _t () =<< writable <$> getPermissions foo
  modifyPermissions foo (\ p -> p { writable = False })
  T.expect _t () =<< not . writable <$> getPermissions foo

  -- test empty path
  modifyPermissions "" id

  where

    checkCurrentDir = do
      -- since the current directory is created by the test runner,
      -- it should be readable, writable, and searchable
      p <- getPermissions "."
      T.expect _t () (readable p)
      T.expect _t () (writable p)
      T.expect _t () (not (executable p))
      T.expect _t () (searchable p)

    checkExecutable = do
      -- 'find' expected to exist on both Windows and POSIX,
      -- though we have no idea if it's writable
      Just f <- findExecutable "find"
      p <- getPermissions f
      T.expect _t () (readable p)
      T.expect _t () (executable p)
      T.expect _t () (not (searchable p))

    checkOrdinary = do
      writeFile "foo" ""
      p <- getPermissions "foo"
      T.expect _t () (readable p)
      T.expect _t () (writable p)
      T.expect _t () (not (executable p))
      T.expect _t () (not (searchable p))

    -- [regression test] (issue #9)
    -- Windows doesn't like trailing path separators
    checkTrailingSlash = do
      createDirectory "bar"
      _ <- getPermissions "bar/"
      pure ()
