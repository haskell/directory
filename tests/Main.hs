module Main (main) where
import qualified Util as T
import qualified FileTime

main :: IO ()
main = T.testMain $ \ _t -> do
  T.isolatedRun _t "FileTime" FileTime.main
