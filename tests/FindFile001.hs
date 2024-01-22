{-# LANGUAGE CPP #-}
module FindFile001 where
#include "util.inl"
import qualified Data.List as List
import System.Directory.Internal
import System.OsPath ((</>))

main :: TestEnv -> IO ()
main _t = do

  createDirectory "bar"
  createDirectory "qux"
  writeFile "foo" ""
  writeFile (so ("bar" </> "foo")) ""
  writeFile (so ("qux" </> "foo")) ":3"

  -- make sure findFile is lazy enough
  T(expectEq) () (Just ("." </> "foo")) =<< findFile ("." : undefined) "foo"

  -- make sure relative paths work
  T(expectEq) () (Just ("." </> "bar" </> "foo")) =<<
    findFile ["."] ("bar" </> "foo")

  T(expectEq) () (Just ("." </> "foo")) =<< findFile [".", "bar"] ("foo")
  T(expectEq) () (Just ("bar" </> "foo")) =<< findFile ["bar", "."] ("foo")

  let f fn = (== ":3") <$> readFile (so fn)
  for_ (List.permutations ["qux", "bar", "."]) $ \ ds -> do

    let (match, noMatch) = List.partition (== "qux") ds
    match0 : _ <- pure match
    noMatch0 : _ <- pure noMatch

    T(expectEq) ds (Just (match0 </> "foo")) =<<
      findFileWith f ds "foo"

    T(expectEq) ds ((</> "foo") <$> match) =<< findFilesWith f ds "foo"

    T(expectEq) ds (Just (noMatch0 </> "foo")) =<<
      findFileWith ((not <$>) . f) ds "foo"

    T(expectEq) ds ((</> "foo") <$> noMatch) =<<
      findFilesWith ((not <$>) . f) ds "foo"

    T(expectEq) ds Nothing =<< findFileWith (\ _ -> return False) ds "foo"

    T(expectEq) ds [] =<< findFilesWith (\ _ -> return False) ds "foo"

  -- make sure absolute paths are handled properly irrespective of 'dirs'
  -- https://github.com/haskell/directory/issues/72
  absPath <- makeAbsolute ("bar" </> "foo")
  absPath2 <- makeAbsolute ("bar" </> "nonexistent")
  T(expectEq) () (Just absPath) =<< findFile [] absPath
  T(expectEq) () Nothing =<< findFile [] absPath2
