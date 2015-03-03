module Main (main) where
import System.Directory
import System.IO.Error (catchIOError)

main = do
  dot <- canonicalizePath "."
  nul <- canonicalizePath "" `catchIOError` \ _ -> return ""
  print (dot == nul)
