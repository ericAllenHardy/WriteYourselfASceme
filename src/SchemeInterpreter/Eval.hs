module SchemeInterpreter.Eval (eval) where

import           Control.Monad.Freer (Member, Eff)
import           SchemeInterpreter.LispVal (LispError(..), LispVal(..)
                                          , FuncApplication(..))
import           SchemeInterpreter.Runtime (Runtime, getVariable, setVariable
                                          , getFunction, throwError)

eval :: Member Runtime r => LispVal -> Eff r LispVal

{- Literals -}
eval (Atom name) = getVariable name >>= 
  \case  
    Just x -> return x
    _      -> throwError (UnboundVar "Unbound variable" name)
eval val @ (String _) = return val
eval val @ (Bool _) = return val
eval val @ (Number _) = return val
eval val @ (Float _) = return val
eval val @ (Char _) = return val
eval val @ (Vector _) = return val
-- DottedList
{- Expressions -}
eval (List [Atom "quote", val]) = return val
eval
  (List [Atom "if", predicate, thenClause, elseClause])
  = eval =<< chooseCase <$> eval predicate
  where
    chooseCase = \case
      Bool False -> elseClause
      _          -> thenClause
eval (List [Atom "set!", Atom name, form]) = getVariable name >>= 
  \case
    Just _ -> storeVariable name form
    _      -> throwError (UnboundVar "Cannot update non-existing variable" name)
eval (List [Atom "define", Atom name, form]) = storeVariable name form
eval (List (Atom func:args)) = traverse eval args >>= apply func
{- Bad Form -}
eval badForm = throwError (BadSpecialForm "Unrecognized special form" badForm)

storeVariable :: Member Runtime r => String -> LispVal -> Eff r LispVal
storeVariable name form = do
  definition <- eval form
  setVariable name definition
  return definition

apply :: Member Runtime r => String -> [LispVal] -> Eff r LispVal
apply name args = getFunction name
  >>= maybe
    (throwError $ NotFunction "Unrecognized primitive function args" name)
    (\f -> case f args of
       FAValue x -> return x
       FAError e -> throwError e)
