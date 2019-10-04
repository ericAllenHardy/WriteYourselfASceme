module SchemeInterpreter.Interpreter (evalString, evalLispFile) where

import           Control.Monad ((<=<))
import           Control.Monad.Trans.State.Strict (evalStateT)
import           SchemeInterpreter.Eval (eval)
import           SchemeInterpreter.Parser (readExpr, readLispFile)
import           SchemeInterpreter.Runtime (Runtime, runRuntime, stdLib, Interpreter)
import           SchemeInterpreter.LispVal (LispVal(..))
import           Control.Monad.Freer (Member, Eff)

evalString :: String -> String
evalString = runInterpreter . runRuntime . (eval <=< readExpr)

evalLispFile :: String -> String
evalLispFile = runInterpreter . runRuntime . (loadExprs <=< readLispFile)
  where
    loadExprs :: Member Runtime r => [LispVal] -> Eff r LispVal
    loadExprs = \case
                  [] -> return (List [])
                  (expr:exprs) -> evalMany expr exprs 
    evalMany :: Member Runtime r => LispVal -> [LispVal] -> Eff r LispVal
    evalMany firstExpr =
      foldl (\runtime expr -> runtime >> eval expr) (eval firstExpr)

runInterpreter :: Interpreter LispVal -> String
runInterpreter interpreter = either showErr show (evalStateT interpreter stdLib)
  where showErr err = "Runtime Error:: " ++ show err

