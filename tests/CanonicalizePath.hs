{-# LANGUAGE CPP #-}
module CanonicalizePath where
import Prelude ()
import System.Directory.Internal.Prelude
import System.Directory.Internal
import System.Directory.OsPath
import TestUtils
import Util (TestEnv)
import qualified Util as T
import System.OsPath ((</>), dropFileName, dropTrailingPathSeparator,
                      normalise, takeFileName)

main :: TestEnv -> IO ()
main _t = do
  dot <- canonicalizePath ""
  dot2 <- canonicalizePath "."
  dot3 <- canonicalizePath "./"
  dot4 <- canonicalizePath "./."
  T.expectEq _t () dot (dropTrailingPathSeparator dot)
  T.expectEq _t () dot dot2
  T.expectEq _t () dot dot3
  T.expectEq _t () dot dot4

  writeFile "bar" ""
  bar <- canonicalizePath "bar"
  bar2 <- canonicalizePath "bar/"
  bar3 <- canonicalizePath "bar/."
  bar4 <- canonicalizePath "bar/./"
  bar5 <- canonicalizePath "./bar"
  bar6 <- canonicalizePath "./bar/"
  bar7 <- canonicalizePath "./bar/."
  T.expectEq _t () bar (normalise (dot </> "bar"))
  T.expectEq _t () bar bar2
  T.expectEq _t () bar bar3
  T.expectEq _t () bar bar4
  T.expectEq _t () bar bar5
  T.expectEq _t () bar bar6
  T.expectEq _t () bar bar7

  createDirectory "foo"
  foo <- canonicalizePath "foo"
  foo2 <- canonicalizePath "foo/"
  foo3 <- canonicalizePath "foo/."
  foo4 <- canonicalizePath "foo/./"
  foo5 <- canonicalizePath "./foo"
  foo6 <- canonicalizePath "./foo/"
  T.expectEq _t () foo (normalise (dot </> "foo"))
  T.expectEq _t () foo foo2
  T.expectEq _t () foo foo3
  T.expectEq _t () foo foo4
  T.expectEq _t () foo foo5
  T.expectEq _t () foo foo6

  -- should not fail for non-existent paths
  fooNon <- canonicalizePath "foo/non-existent"
  fooNon2 <- canonicalizePath "foo/non-existent/"
  fooNon3 <- canonicalizePath "foo/non-existent/."
  fooNon4 <- canonicalizePath "foo/non-existent/./"
  fooNon5 <- canonicalizePath "./foo/non-existent"
  fooNon6 <- canonicalizePath "./foo/non-existent/"
  fooNon7 <- canonicalizePath "./foo/./non-existent"
  fooNon8 <- canonicalizePath "./foo/./non-existent/"
  T.expectEq _t () fooNon (normalise (foo </> "non-existent"))
  T.expectEq _t () fooNon fooNon2
  T.expectEq _t () fooNon fooNon3
  T.expectEq _t () fooNon fooNon4
  T.expectEq _t () fooNon fooNon5
  T.expectEq _t () fooNon fooNon6
  T.expectEq _t () fooNon fooNon7
  T.expectEq _t () fooNon fooNon8

  -- make sure ".." gets expanded properly by 'toExtendedLengthPath'
  -- (turns out this test won't detect the problem because GetFullPathName
  -- would expand them for us if we don't, but leaving it here anyway)
  T.expectEq _t () foo =<< canonicalizePath (foo </> ".." </> "foo")

  supportsSymbolicLinks <- supportsSymlinks
  when supportsSymbolicLinks $ do

    let barQux = dot </> "bar" </> "qux"

    -- note: this also checks that "../bar" gets normalized to "..\\bar"
    --       since Windows does not like "/" in symbolic links targets
    createFileLink "../bar" "foo/bar"
    T.expectEq _t () bar =<< canonicalizePath "foo/bar"
    T.expectEq _t () barQux =<< canonicalizePath "foo/bar/qux"

    createDirectoryLink "foo" "lfoo"
    T.expectEq _t () foo =<< canonicalizePath "lfoo"
    T.expectEq _t () foo =<< canonicalizePath "lfoo/"
    T.expectEq _t () bar =<< canonicalizePath "lfoo/bar"
    T.expectEq _t () barQux =<< canonicalizePath "lfoo/bar/qux"

    -- create a haphazard chain of links
    createDirectoryLink "./../foo/../foo/." "./foo/./somelink3"
    createDirectoryLink ".././foo/somelink3" "foo/somelink2"
    createDirectoryLink "./foo/somelink2" "somelink"
    T.expectEq _t () foo =<< canonicalizePath "somelink"

    -- regression test for #64
    createFileLink "../foo/non-existent" "foo/qux"
    removeDirectoryLink "foo/somelink3" -- break the chain made earlier
    qux <- canonicalizePath "foo/qux"
    T.expectEq _t () qux =<< canonicalizePath "foo/non-existent"
    T.expectEq _t () (foo </> "somelink3") =<< canonicalizePath "somelink"

    -- make sure it can handle loops
    createFileLink "loop1" "loop2"
    createFileLink "loop2" "loop1"
    loop1 <- canonicalizePath "loop1"
    loop2 <- canonicalizePath "loop2"
    T.expectEq _t () loop1 (normalise (dot </> "loop1"))
    T.expectEq _t () loop2 (normalise (dot </> "loop2"))

    -- make sure ".." gets expanded properly by 'toExtendedLengthPath'
    createDirectoryLink (foo </> ".." </> "foo") "foolink"
    _ <- listDirectory "foolink" -- make sure directory is accessible
    T.expectEq _t () foo =<< canonicalizePath "foolink"

  caseInsensitive <-
    (False <$ createDirectory "FOO")
      `catch` \ e ->
        if isAlreadyExistsError e
        then pure True
        else throwIO e

  -- if platform is case-insensitive, we expect case to be canonicalized too
  when caseInsensitive $ do
    foo7 <- canonicalizePath "FOO"
    foo8 <- canonicalizePath "FOO/"
    T.expectEq _t () foo foo7
    T.expectEq _t () foo foo8

    fooNon9 <- canonicalizePath "FOO/non-existent"
    fooNon10 <- canonicalizePath "fOo/non-existent/"
    fooNon11 <- canonicalizePath "foO/non-existent/."
    fooNon12 <- canonicalizePath "FoO/non-existent/./"
    fooNon13 <- canonicalizePath "./fOO/non-existent"
    fooNon14 <- canonicalizePath "./FOo/non-existent/"
    cfooNon15 <- canonicalizePath "./FOO/./NON-EXISTENT"
    cfooNon16 <- canonicalizePath "./FOO/./NON-EXISTENT/"
    T.expectEq _t () fooNon fooNon9
    T.expectEq _t () fooNon fooNon10
    T.expectEq _t () fooNon fooNon11
    T.expectEq _t () fooNon fooNon12
    T.expectEq _t () fooNon fooNon13
    T.expectEq _t () fooNon fooNon14
    T.expectEq _t () fooNon (dropFileName cfooNon15 <>
                           (os (toLower <$> so (takeFileName cfooNon15))))
    T.expectEq _t () fooNon (dropFileName cfooNon16 <>
                           (os (toLower <$> so (takeFileName cfooNon16))))
    T.expectNe _t () fooNon cfooNon15
    T.expectNe _t () fooNon cfooNon16

    setCurrentDirectory "foo"
    foo9 <- canonicalizePath "../FOO"
    foo10 <- canonicalizePath "../FOO/"
    T.expectEq _t () foo foo9
    T.expectEq _t () foo foo10

  let isWindows =
#if defined(mingw32_HOST_OS)
        True
#else
        False
#endif

  when isWindows $ do
    -- https://github.com/haskell/directory/issues/170
    T.expectEq _t () "\\\\localhost" =<< canonicalizePath "\\\\localhost"
    -- https://github.com/haskell/directory/issues/206
    T.expectEq _t () "\\\\localhost\\C$" =<<
      canonicalizePath "\\\\localhost\\C$\\.."
