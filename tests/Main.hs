module Main (main) where
import qualified Util as T
import qualified CanonicalizePath
import qualified CurrentDirectory001
import qualified Directory001
import qualified DoesDirectoryExist001
import qualified FileTime
import qualified T8482

main :: IO ()
main = T.testMain $ \ _t -> do
  T.isolatedRun _t "CanonicalizePath" CanonicalizePath.main
  T.isolatedRun _t "CurrentDirectory001" CurrentDirectory001.main
  T.isolatedRun _t "Directory001" Directory001.main
  T.isolatedRun _t "DoesDirectoryExist001" DoesDirectoryExist001.main
  T.isolatedRun _t "FileTime" FileTime.main
  T.isolatedRun _t "T8482" T8482.main
