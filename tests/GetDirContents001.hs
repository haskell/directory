{-# LANGUAGE CPP #-}
module GetDirContents001 where
#include "util.inl"
import System.Directory.Internal
import System.OsPath ((</>))
import qualified Data.List as List

main :: TestEnv -> IO ()
main _t = do
  createDirectory dir
  T(expectEq) () specials . List.sort =<<
    getDirectoryContents dir
  T(expectEq) () [] . List.sort =<<
    listDirectory dir
  names <- for [1 .. 100 :: Int] $ \ i -> do
    let name = "f" <> os (show i)
    writeFile (so (dir </> name)) ""
    return name
  T(expectEq) () (List.sort (specials <> names)) . List.sort =<<
    getDirectoryContents dir
  T(expectEq) () (List.sort names) . List.sort =<<
    listDirectory dir
  where dir      = "dir"
        specials = [".", ".."]
