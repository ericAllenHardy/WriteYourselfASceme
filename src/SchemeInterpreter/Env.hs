{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module SchemeInterpreter.Env
    ( Env
      --   , addVariable
      --   , addFunction
    , getVariable
    , getFunction
    , currentEnv
    , throwError
    , runEnv
    , NameLookup(..)
    , stdLib
    , NameValue(..)) where

import qualified Data.Map.Strict as M
import           Control.Monad.Trans.State.Strict (StateT, get, put)
import qualified Control.Monad.Except as E
import           SchemeInterpreter.LispVal (LispError(..), LispVal(..)
                                          , FuncApplication)
import           SchemeInterpreter.StdLib (functionEnv)
import           Control.Monad.Freer (Member, send, run)
import           Control.Monad.Freer.Internal (extract, Eff(..), qApp)

type LispFunction = [LispVal] -> FuncApplication LispVal

data NameValue = Function LispFunction
               | Variable LispVal

newtype NameLookup = NameLookup { names :: M.Map String NameValue }

stdLib :: NameLookup
stdLib = buildStdLib functionEnv
  where
    buildStdLib = NameLookup . M.map Function

type Interpreter = StateT NameLookup (Either LispError)

data Env a where
  --    AddVariable ::String -> LispVal -> Env ()
  --    AddFunction ::String -> ([LispVal] -> LispRuntime LispVal) -> Env ()
  Interpret :: LispVal -> Env LispVal
  GetVariable :: String -> Env (Maybe LispVal)
  GetFunction :: String -> Env (Maybe LispFunction)
  CurrentEnv :: Env NameLookup
  ThrowError :: LispError -> Env a

-- addVariable name x = send (AddVariable name x)
-- addFunction name f = send (AddFunction name f)
getVariable name = send (GetVariable name)

getFunction name = send (GetFunction name)

currentEnv :: Member Env r => Eff r NameLookup
currentEnv = send CurrentEnv

throwError err = send (ThrowError err)

runEnv :: Eff '[Env] a -> Interpreter a
runEnv (Val x) = return x
runEnv (E x next) = case extract x of
  Interpret val    -> continueWith val
  --    AddVariable name x    -> undefined
  --    AddFunction name f    -> undefined
  GetVariable name -> do
    nameLookup <- get
    let val = case M.lookup name (names nameLookup) of
          Just (Variable v) -> Just v
          _ -> Nothing
    continueWith val
  GetFunction name -> do
    nameLookup <- get
    let val = case M.lookup name (names nameLookup) of
          Just (Function f) -> Just f
          _ -> Nothing
    continueWith val
  CurrentEnv       -> get >>= continueWith
  ThrowError err   -> E.throwError err
  where
    continueWith = runEnv . qApp next
