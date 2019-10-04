module Main where

import           REPL (runRepl)
import           SchemeInterpreter.Interpreter (evalLispFile)
import           System.Environment (getArgs)
import           Control.Monad ((>=>))

main :: IO ()
main = getArgs >>= runSubCommand

runSubCommand :: [String] -> IO ()
runSubCommand ["repl"] = runRepl
runSubCommand ["file", file] = evalFile file
runSubCommand _ = print helpStr
  where
    helpStr = "Options: repl, file {filepath}"

evalFile :: String -> IO ()
evalFile = readFile >=> print . evalLispFile