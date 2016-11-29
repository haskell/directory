{-# LANGUAGE CPP #-}
module CanonicalizePath where
#include "util.inl"
import System.Directory
import System.FilePath ((</>), dropTrailingPathSeparator, normalise)

main :: TestEnv -> IO ()
main _t = do
  dot <- canonicalizePath ""
  dot2 <- canonicalizePath "."
  dot3 <- canonicalizePath "./"
  dot4 <- canonicalizePath "./."
  T(expectEq) () dot (dropTrailingPathSeparator dot)
  T(expectEq) () dot dot2
  T(expectEq) () dot dot3
  T(expectEq) () dot dot4

  writeFile "bar" ""
  bar <- canonicalizePath "bar"
  bar2 <- canonicalizePath "bar/"
  bar3 <- canonicalizePath "bar/."
  bar4 <- canonicalizePath "bar/./"
  bar5 <- canonicalizePath "./bar"
  bar6 <- canonicalizePath "./bar/"
  bar7 <- canonicalizePath "./bar/."
  T(expectEq) () bar (normalise (dot </> "bar"))
  T(expectEq) () bar bar2
  T(expectEq) () bar bar3
  T(expectEq) () bar bar4
  T(expectEq) () bar bar5
  T(expectEq) () bar bar6
  T(expectEq) () bar bar7

  createDirectory "foo"
  foo <- canonicalizePath "foo"
  foo2 <- canonicalizePath "foo/"
  foo3 <- canonicalizePath "foo/."
  foo4 <- canonicalizePath "foo/./"
  foo5 <- canonicalizePath "./foo"
  foo6 <- canonicalizePath "./foo/"
  T(expectEq) () foo (normalise (dot </> "foo"))
  T(expectEq) () foo foo2
  T(expectEq) () foo foo3
  T(expectEq) () foo foo4
  T(expectEq) () foo foo5
  T(expectEq) () foo foo6

  -- should not fail for non-existent paths
  fooNon <- canonicalizePath "foo/non-existent"
  fooNon2 <- canonicalizePath "foo/non-existent/"
  fooNon3 <- canonicalizePath "foo/non-existent/."
  fooNon4 <- canonicalizePath "foo/non-existent/./"
  fooNon5 <- canonicalizePath "./foo/non-existent"
  fooNon6 <- canonicalizePath "./foo/non-existent/"
  fooNon7 <- canonicalizePath "./foo/./non-existent"
  fooNon8 <- canonicalizePath "./foo/./non-existent/"
  T(expectEq) () fooNon (normalise (foo </> "non-existent"))
  T(expectEq) () fooNon fooNon2
  T(expectEq) () fooNon fooNon3
  T(expectEq) () fooNon fooNon4
  T(expectEq) () fooNon fooNon5
  T(expectEq) () fooNon fooNon6
  T(expectEq) () fooNon fooNon7
  T(expectEq) () fooNon fooNon8
