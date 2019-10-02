{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}

module SchemeInterpreter.Eval (eval) where

import           Control.Monad.Freer (Member, Eff)
import qualified Data.Map.Strict as M
import           SchemeInterpreter.LispVal (LispError(..), LispVal(..)
                                          , FuncApplication(..))
import           SchemeInterpreter.Runtime (Runtime, getVariable, getFunction
                                          , currentRuntime, throwError)

eval :: Member Runtime r => LispVal -> Eff r LispVal
eval val @ (String _) = return val
eval val @ (Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = do
  result <- eval pred
  eval
    $ case result of
      Bool False -> alt
      _          -> conseq
eval (List (Atom func:args)) = do
  reducedArgs <- traverse eval args
  apply func reducedArgs
eval badForm = throwError (BadSpecialForm "Unrecognized special form" badForm)

apply :: Member Runtime r => String -> [LispVal] -> Eff r LispVal
apply name args = maybe
  (throwError $ NotFunction "Unrecognized primitive function args" name)
  (\f -> case f args of
     FAValue x -> return x
     FAError e -> throwError e)
  =<< getFunction name
