{-# LANGUAGE CPP #-}
module MakeAbsolute where
import Prelude ()
import System.Directory.Internal.Prelude
import System.Directory.OsPath
import TestUtils ()
import Util (TestEnv)
import qualified Util as T
import System.OsPath ((</>), addTrailingPathSeparator,
                      dropTrailingPathSeparator, normalise)
#if defined(mingw32_HOST_OS)
import System.Directory.Internal
import System.OsPath (takeDrive, toChar, unpack)
#endif

main :: TestEnv -> IO ()
main _t = do
  dot <- makeAbsolute ""
  dot2 <- makeAbsolute "."
  dot3 <- makeAbsolute "./."
  T.expectEq _t () dot (dropTrailingPathSeparator dot)
  T.expectEq _t () dot dot2
  T.expectEq _t () dot dot3

  sdot <- makeAbsolute "./"
  sdot2 <- makeAbsolute "././"
  T.expectEq _t () sdot (addTrailingPathSeparator sdot)
  T.expectEq _t () sdot sdot2

  foo <- makeAbsolute "foo"
  foo2 <- makeAbsolute "foo/."
  foo3 <- makeAbsolute "./foo"
  T.expectEq _t () foo (normalise (dot </> "foo"))
  T.expectEq _t () foo foo2
  T.expectEq _t () foo foo3

  sfoo <- makeAbsolute "foo/"
  sfoo2 <- makeAbsolute "foo/./"
  sfoo3 <- makeAbsolute "./foo/"
  T.expectEq _t () sfoo (normalise (dot </> "foo/"))
  T.expectEq _t () sfoo sfoo2
  T.expectEq _t () sfoo sfoo3

#if defined(mingw32_HOST_OS)
  cwd <- getCurrentDirectory
  let driveLetter = toUpper (toChar (head (unpack (takeDrive cwd))))
  let driveLetter' = if driveLetter == 'Z' then 'A' else succ driveLetter
  drp1 <- makeAbsolute (os (driveLetter : ":foobar"))
  drp2 <- makeAbsolute (os (driveLetter' : ":foobar"))
  T.expectEq _t () drp1 =<< makeAbsolute "foobar"
  T.expectEq _t () drp2 (os (driveLetter' : ":\\foobar"))
#endif
