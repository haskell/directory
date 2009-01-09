import System.Directory

main = do
  p <- getPermissions "."
  print p
  p <- getPermissions "getPermissions001.hs"
  print p
#ifndef mingw32_HOST_OS
  p <- getPermissions "getPermissions001"
#else
  p <- getPermissions "getPermissions001.exe"
#endif
  print p
