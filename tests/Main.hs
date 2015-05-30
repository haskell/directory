module Main (main) where
import qualified Util as T
import qualified ModificationTime

main :: IO ()
main = T.testMain $ \ _t -> do
  T.isolatedRun _t "ModificationTime" ModificationTime.main
