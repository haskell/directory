{-# LANGUAGE CPP #-}
import System.Directory

main = do
#ifdef mingw32_HOST_OS
  let exe = ".exe"
#else
  let exe = ""
#endif
  p <- getPermissions "."
  print p
  p <- getPermissions "getPermissions001.hs"
  print p
  p <- getPermissions ("getPermissions001" ++ exe)
  print p

  -- issue #9: Windows doesn't like trailing path separators
  _ <- getPermissions "../tests/"

  return ()
