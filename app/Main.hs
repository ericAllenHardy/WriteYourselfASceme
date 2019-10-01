module Main where

import           Data.Maybe                     ( listToMaybe )
import           REPL                           ( runRepl )
import           SchemeInterpreter.Eval         ( eval )
import           SchemeInterpreter.StdLib       ( functionEnv )
import           SchemeInterpreter.Parser       ( readExpr )
import           System.Environment             ( getArgs )
import           Control.Monad                  ( (>=>) )

main :: IO ()
main = do
  args <- getArgs
  runSubCommand args

runSubCommand :: [String] -> IO ()
runSubCommand ["repl"]       = runRepl
runSubCommand ["file", file] = evalFile file
runSubCommand _              = print helpStr
  where helpStr = "Options: repl, file {filepath}"

evalFile :: String -> IO ()
evalFile = readFile >=> print . evalString

evalString :: String -> String
evalString s = either show show (eval functionEnv =<< readExpr s)
