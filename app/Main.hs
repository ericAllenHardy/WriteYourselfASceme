module Main where

import           SchemeInterpreter.REPL         ( repl )
import           SchemeInterpreter.EvalFile     ( evalFile )
import           System.Environment             ( getArgs )


main :: IO ()
main = getArgs >>= runSubCommand

runSubCommand :: [String] -> IO ()
runSubCommand ["repl"]       = repl
runSubCommand ["file", file] = evalFile file
runSubCommand _              = print helpStr
  where helpStr = "Options: repl, file {filepath}"
