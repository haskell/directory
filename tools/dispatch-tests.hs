{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Main (main) where
import Foreign (Ptr)
import Foreign.C (CChar(..), CInt(..), withCString)
import Data.Functor ((<$>))
import System.Directory ()     -- to make sure `directory` is built beforehand
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitSuccess, ExitFailure), exitWith)

main :: IO ()
main = do

  -- check if 'cabal exec' is supported (didn't exist until 1.20)
  cabalExecTest <- rawSystem "sh" ["-c", "cabal >/dev/null 2>&1 exec true"]

  -- execute in the Cabal sandbox environment if possible
  let prefix = case cabalExecTest of
                 ExitSuccess   -> ["cabal", "exec", "--"]
                 ExitFailure _ -> []

  args <- getArgs
  let command : arguments = prefix ++ ["sh", "tools/run-tests"] ++ args
  exitWith =<< normalizeExitCode <$> rawSystem command arguments

makeExitCode :: Int -> ExitCode
makeExitCode 0 = ExitSuccess
makeExitCode e = ExitFailure e

-- on Linux the exit code is right-shifted by 8 bits, causing exit codes to be
-- rather large; older versions of GHC don't seem to handle that well in
-- `exitWith`
normalizeExitCode :: ExitCode -> ExitCode
normalizeExitCode  ExitSuccess    = ExitSuccess
normalizeExitCode (ExitFailure _) = ExitFailure 1

-- we can't use the `process` library as it causes a dependency cycle with
-- Cabal, so we reinvent the wheel here in a simplistic way; this will
-- probably break with non-ASCII characters on Windows
rawSystem :: String -> [String] -> IO ExitCode
rawSystem cmd args  =
  withCString (quoteCmdArgs (cmd : args)) $ \ c_command ->
  makeExitCode . fromIntegral <$> c_system c_command

-- handle the different quoting rules in CMD.EXE vs POSIX shells
quoteCmdArgs :: [String] -> String
quoteCmdArgs cmdArgs =
#ifdef mingw32_HOST_OS
  -- the arcane quoting rules require us to add an extra set of quotes
  -- around the entire thing: see `help cmd` or look at
  -- https://superuser.com/a/238813
  "\"" ++ unwords (quote <$> cmdArgs) ++ "\""
  where quote s = "\"" ++ replaceElem '"' "\"\"" s ++ "\""
#else
  unwords (quote <$> cmdArgs)
  where quote s = "'" ++ replaceElem '\'' "'\\''" s ++ "'"
#endif

replaceElem :: Eq a => a -> [a] -> [a] -> [a]
replaceElem match repl = concat . (replace <$>)
  where replace c | c == match = repl
                  | otherwise  = [c]

foreign import ccall safe "stdlib.h system" c_system :: Ptr CChar -> IO CInt
