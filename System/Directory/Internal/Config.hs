{-# LANGUAGE CPP #-}
#if defined(ghcjs_HOST_OS)
{-# LANGUAGE JavaScriptFFI, GHCForeignImportPrim #-}
#else
#include <HsDirectoryConfig.h>
#endif
module System.Directory.Internal.Config where

#if defined(ghcjs_HOST_OS)
import GHCJS.Prim
#endif

exeExtension :: String
#if defined(ghcjs_HOST_OS)
exeExtension = fromJSString js_exeExtension
foreign import javascript unsafe
  "h$directory_exeExtension()"
  js_exeExtension :: JSVal
#else
exeExtension = EXE_EXTENSION
#endif
-- We avoid using #const_str from hsc because it breaks cross-compilation
-- builds, so we use this ugly workaround where we simply paste the C string
-- literal directly in here.  This will probably break if the EXE_EXTENSION
-- contains strange characters, but hopefully no sane OS would ever do that.
