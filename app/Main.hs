module Main where

import           Data.Maybe               (listToMaybe)
import           SchemeInterpreter.Eval   (eval, functionEnv)
import           SchemeInterpreter.Parser (readExpr)
import           System.Environment       (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case listToMaybe args of
    Just file ->
      let result = eval functionEnv =<< readExpr file
       in print (either show show result)
    Nothing -> print "Please give a file to eval"
