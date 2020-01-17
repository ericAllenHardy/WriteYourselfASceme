{-# LANGUAGE GADTs #-}

module SchemeInterpreter.Interpreter (Interpreter, interpret, interpretRuntime) where

import           Control.Monad.State (MonadState, get, modify)
import           Control.Monad.Except (throwError, MonadError)
import           Control.Monad.Freer.Internal (extract, Eff(..), qApp)
import           Control.Monad.Trans.State.Strict (StateT, runStateT)
import           SchemeInterpreter.LispVal
import           SchemeInterpreter.LispComp (LispComp(..))
import           SchemeInterpreter.Runtime.Env (RuntimeEnv, getFunction, getVariable, addVariable)


newtype Interpreter a = Interpreter {runInterpreter :: StateT RuntimeEnv (Either LispError) a}
  deriving (Monad, Applicative, Functor, MonadState RuntimeEnv
          , MonadError LispError)

interpret ::  RuntimeEnv -> Eff '[LispComp] LispVal -> Either LispError (LispVal, RuntimeEnv)
interpret env = (`runStateT` env) . runInterpreter . interpretRuntime

interpretRuntime :: Eff '[LispComp] a -> Interpreter a
interpretRuntime (Val x) = return x
interpretRuntime (E x next) = case extract x of
  GetVariable name        -> get >>= \env -> continueWith (getVariable env name)
  GetFunction name        -> get >>= \env -> continueWith (getFunction env name)
  CurrentRuntime          -> get >>= continueWith
  SetVariable name val    -> modify (addVariable name val) >> continueWith ()
  DefineVariable name val -> modify (addVariable name val) >> continueWith ()
  ThrowError err          -> throwError err
  where
    continueWith = interpretRuntime . qApp next