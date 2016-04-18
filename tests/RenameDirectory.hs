{-# LANGUAGE CPP #-}
module RenameDirectory where
#include "util.inl"
import System.Directory

main :: TestEnv -> IO ()
main _t = do
  createDirectory "a"
  T(expectEq) () ["a"] =<< listDirectory "."
  renameDirectory "a" "b"
  T(expectEq) () ["b"] =<< listDirectory "."
