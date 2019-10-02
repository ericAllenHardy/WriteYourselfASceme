module REPL (runRepl) where

import           Control.Monad ((>=>))
import           Control.Monad.Trans.State.Strict (evalStateT)
import           SchemeInterpreter.Env (runEnv, NameLookup(..), NameValue(..)
                                      , stdLib)
import           SchemeInterpreter.Eval (eval)
import           SchemeInterpreter.Parser (readExpr)
import           System.IO (hFlush, stdout)
import           Control.FromSum (fromEither)
import qualified Data.Map.Strict as M

evalString :: String -> IO String
evalString expr =
  let result = evalStateT (runEnv $ eval =<< readExpr expr) stdLib
  in return (extractValue result)
  where
    extractValue = either show show

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

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

