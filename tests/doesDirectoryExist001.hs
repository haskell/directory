{-# LANGUAGE CPP #-}
-- !!! "/" was not recognised as a directory in 6.0.x
import System.Directory

#ifdef mingw32_HOST_OS
root = "C:\\"
#else
root = "/"
#endif

main = doesDirectoryExist root >>= print
