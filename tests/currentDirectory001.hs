
import System.Directory (getCurrentDirectory, setCurrentDirectory,
                         createDirectory, removeDirectory,
                         getDirectoryContents)

main :: IO ()
main = do
    oldpwd <- getCurrentDirectory
    createDirectory dir
    setCurrentDirectory dir
    ~[n1, n2] <- getDirectoryContents "."
    if dot n1 && dot n2 
     then do
        setCurrentDirectory oldpwd
        removeDirectory dir
        putStr "Okay\n"
      else
        ioError (userError "Oops")

dot :: String -> Bool
dot "." = True
dot ".." = True
dot _ = False

dir :: FilePath
dir = "currentDirectory001-dir"

