{-# LANGUAGE NoImplicitPrelude #-}
-- Simplistic test suite for now. Worthwhile to add a dependency on a
-- test framework at some point.
module Main (main) where

import qualified Data.Set         as Set
import           Prelude          (IO, error, fmap, return, show, (==))
import           System.Directory (getDirectoryContents)

main :: IO ()
main = do
    let expected = Set.fromList
            [ "."
            , ".."
            , "main.hs"
            ]
    actual <- fmap Set.fromList (getDirectoryContents "test")
    if expected == actual
        then return ()
        else error (show (expected, actual))
