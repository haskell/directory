{-# LANGUAGE CPP #-}
module CanonicalizePath where
#include "util.inl"
import System.Directory
import System.FilePath ((</>), hasTrailingPathSeparator, normalise)

main :: TestEnv -> IO ()
main _t = do
  dot' <- canonicalizePath "./"
  dot <- canonicalizePath "."
  nul <- canonicalizePath ""
  T(expectEq) () dot nul
  T(expect) dot (not (hasTrailingPathSeparator dot))
  T(expect) dot' (hasTrailingPathSeparator dot')

  writeFile "bar" ""
  bar <- canonicalizePath "bar"
  T(expectEq) () bar (normalise (dot </> "bar"))

  createDirectory "foo"
  foo <- canonicalizePath "foo/"
  T(expectEq) () foo (normalise (dot </> "foo/"))

  -- should not fail for non-existent paths
  fooNon <- canonicalizePath "foo/non-existent"
  T(expectEq) () fooNon (normalise (foo </> "non-existent"))
