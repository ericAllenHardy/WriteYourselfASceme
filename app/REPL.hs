module REPL (runRepl) where

import           Control.Monad ((>=>))
import           Control.Monad.Trans.State.Strict (evalStateT)
import qualified SchemeInterpreter.Runtime as R
import           SchemeInterpreter.Eval (eval)
import           SchemeInterpreter.Parser (readExpr)
import           System.IO (hFlush, stdout)

-- import           Control.Monad.Freer (Member, Eff)
evalString :: String -> IO String
evalString expr =
  let result = evalStateT (R.runRuntime $ eval =<< readExpr expr) R.stdLib
  in return (extractValue result)
  where
    extractValue = either show show

until_ :: (a -> Bool) -> IO a -> (a -> IO ()) -> IO ()
until_ predicate prompt action = do
  result <- prompt
  if predicate result
    then return ()
    else action result >> until_ predicate prompt action

runRepl :: IO ()
runRepl = do
  print "Type 'exit' to close"
  until_ (== "exit") (readPrompt "Lisp>>> ") evalAndPrint
  where
    readPrompt :: String -> IO String
    readPrompt prompt = flushStr prompt >> getLine

    flushStr :: String -> IO ()
    flushStr str = putStr str >> hFlush stdout

    evalAndPrint :: String -> IO ()
    evalAndPrint = evalString >=> putStrLn

