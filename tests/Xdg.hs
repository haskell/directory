{-# LANGUAGE CPP #-}
module Xdg where
#if MIN_VERSION_base(4, 7, 0)
import qualified Data.List as List
import System.Environment (setEnv, unsetEnv)
import System.FilePath (searchPathSeparator)
#if !defined(mingw32_HOST_OS)
import System.FilePath ((</>))
#endif
#endif
#include "util.inl"

main :: TestEnv -> IO ()
main _t = do

  -- smoke tests
  _ <- getXdgDirectoryList XdgDataDirs
  _ <- getXdgDirectoryList XdgConfigDirs

  T(expect) () True -- avoid warnings about redundant imports

  -- setEnv, unsetEnv require base 4.7.0.0+
#if MIN_VERSION_base(4, 7, 0)
#if !defined(mingw32_HOST_OS)
  unsetEnv "XDG_CONFIG_HOME"
  home <- getHomeDirectory
  T(expectEq) () (home </> ".config/mow") =<< getXdgDirectory XdgConfig "mow"
#endif

  -- unset variables, so env doesn't affect test running
  unsetEnv "XDG_DATA_HOME"
  unsetEnv "XDG_CONFIG_HOME"
  unsetEnv "XDG_CACHE_HOME"
  unsetEnv "XDG_STATE_HOME"
  xdgData   <- getXdgDirectory XdgData   "ff"
  xdgConfig <- getXdgDirectory XdgConfig "oo"
  xdgCache  <- getXdgDirectory XdgCache  "rk"
  xdgState  <- getXdgDirectory XdgState  "aa"

  -- non-absolute paths are ignored, and the fallback is used
  setEnv "XDG_DATA_HOME"   "ar"
  setEnv "XDG_CONFIG_HOME" "aw"
  setEnv "XDG_CACHE_HOME"  "ba"
  setEnv "XDG_STATE_HOME"  "uw"
  T(expectEq) () xdgData   =<< getXdgDirectory XdgData   "ff"
  T(expectEq) () xdgConfig =<< getXdgDirectory XdgConfig "oo"
  T(expectEq) () xdgCache  =<< getXdgDirectory XdgCache  "rk"
  T(expectEq) () xdgState  =<< getXdgDirectory XdgState  "aa"

  unsetEnv "XDG_CONFIG_DIRS"
  unsetEnv "XDG_DATA_DIRS"
  _xdgConfigDirs <- getXdgDirectoryList XdgConfigDirs
  _xdgDataDirs <- getXdgDirectoryList XdgDataDirs

#if !defined(mingw32_HOST_OS)
  T(expectEq) () ["/etc/xdg"] _xdgConfigDirs
  T(expectEq) () ["/usr/local/share/", "/usr/share/"] _xdgDataDirs
#endif

  setEnv "XDG_DATA_DIRS" (List.intercalate [searchPathSeparator] ["/a", "/b"])
  setEnv "XDG_CONFIG_DIRS" (List.intercalate [searchPathSeparator] ["/c", "/d"])
  T(expectEq) () ["/a", "/b"] =<< getXdgDirectoryList XdgDataDirs
  T(expectEq) () ["/c", "/d"] =<< getXdgDirectoryList XdgConfigDirs
#endif

  return ()
