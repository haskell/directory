module Main (main) where

import Distribution.Simple
import Distribution.Simple.Program (simpleProgram)

main :: IO ()
main = defaultMainWithHooks autoconfUserHooks
       { hookedPrograms = [simpleProgram "python", simpleProgram "which"] }
