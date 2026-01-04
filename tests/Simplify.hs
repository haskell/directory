{-# LANGUAGE CPP #-}
module Simplify where
import Prelude ()
import System.Directory.Internal.Prelude
import System.Directory.Internal (simplifyWindows)
import System.Directory.OsPath
import TestUtils ()
import Util (TestEnv)
import qualified Util as T
import System.OsPath (normalise)

main :: TestEnv -> IO ()
main _t = do
  T.expectIOErrorType _t () (const True) (setCurrentDirectory "")
  T.expectEq _t () (simplifyWindows "") ""
  T.expectEq _t () (simplifyWindows ".") "."
  T.expectEq _t () (simplifyWindows "a///b") (normalise "a/b")
  T.expectEq _t () (simplifyWindows "./a//b") (normalise "a/b")
  T.expectEq _t () (simplifyWindows "a/../../../b/.") (normalise "../../b")
  T.expectEq _t () (simplifyWindows "a/.././b/./") (normalise "b/")
  T.expectEq _t () (simplifyWindows "C:/a/../b") (normalise "C:/b")
  T.expectEq _t () (simplifyWindows "\\\\?\\./a\\../b") "\\\\?\\./a\\../b"
  T.expectEq _t () (simplifyWindows "C:/a") (normalise "C:/a")
  T.expectEq _t () (simplifyWindows "/a") (normalise "/a")
#ifdef mingw32_HOST_OS
  T.expectEq _t () (simplifyWindows "C:") "C:"
  T.expectEq _t () (simplifyWindows "c:\\\\") "C:\\"
  T.expectEq _t () (simplifyWindows "C:.") "C:"
  T.expectEq _t () (simplifyWindows "C:.\\\\") "C:.\\"
  T.expectEq _t () (simplifyWindows "C:..") "C:.."
  T.expectEq _t () (simplifyWindows "C:..\\") "C:..\\"
  T.expectEq _t () (simplifyWindows "C:\\.\\") "C:\\"
  T.expectEq _t () (simplifyWindows "C:\\a") "C:\\a"
  T.expectEq _t () (simplifyWindows "C:\\a\\\\b\\") "C:\\a\\b\\"
  T.expectEq _t () (simplifyWindows "\\\\a\\b") "\\\\a\\b"
  T.expectEq _t () (simplifyWindows "//a\\b/c/./d") "\\\\a\\b/c/./d"
  T.expectEq _t () (simplifyWindows "/.") "\\"
  T.expectEq _t () (simplifyWindows "/./") "\\"
  T.expectEq _t () (simplifyWindows "/../") "\\"
  T.expectEq _t () (simplifyWindows "\\a\\.") "\\a"
  T.expectEq _t () (simplifyWindows "//?") "\\\\?"
  T.expectEq _t () (simplifyWindows "//?\\") "\\\\?\\"
  T.expectEq _t () (simplifyWindows "//?/a/b") "\\\\?\\a/b"
#endif
