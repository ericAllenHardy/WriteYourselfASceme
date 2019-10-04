{-# LANGUAGE GADTs #-}
module SchemeInterpreter.Runtime
    ( Runtime
      --   , addVariable
      --   , addFunction
    , getVariable
    , setVariable
    , defineVariable
    , getFunction
    , currentRuntime
    , throwError
    , runRuntime
    , NameLookup(..)
    , stdLib
    , NameValue(..)
    , Interpreter) where

import qualified Data.Map.Strict as M
import           Control.Monad.Trans.State.Strict (StateT, get, modify)
import qualified Control.Monad.Except as E
import           SchemeInterpreter.LispVal (LispError(..), LispVal(..)
                                          , LispFunction)
import           SchemeInterpreter.StdLib (stdLibFunctions)
import           Control.Monad.Freer (Member, send)
import           Control.Monad.Freer.Internal (extract, Eff(..), qApp)

data NameValue = Function LispFunction
               | Variable LispVal

newtype NameLookup = NameLookup { names :: M.Map String NameValue }

stdLib :: NameLookup
stdLib = buildStdLib stdLibFunctions
  where
    buildStdLib = NameLookup . M.map Function

type Interpreter = StateT NameLookup (Either LispError)

data Runtime a where
  --    AddVariable ::String -> LispVal -> Runtime ()
  --    AddFunction ::String -> ([LispVal] -> LispRuntime LispVal) -> Runtime ()
  Interpret :: LispVal -> Runtime LispVal
  GetVariable :: String -> Runtime (Maybe LispVal)
  SetVariable :: String -> LispVal -> Runtime () -- assumes var exists aleady
  DefineVariable :: String -> LispVal -> Runtime () -- upsert
  -- DefineFunction :: String -> LispFunction -> Runtime ()
  GetFunction :: String -> Runtime (Maybe LispFunction)
  CurrentRuntime :: Runtime NameLookup
  ThrowError :: LispError -> Runtime a

-- addVariable name x = send (AddVariable name x)
-- addFunction name f = send (AddFunction name f)
getVariable :: Member Runtime r => String -> Eff r (Maybe LispVal)
getVariable name = send (GetVariable name)

setVariable :: Member Runtime r => String -> LispVal -> Eff r ()
setVariable name val = send (SetVariable name val)

defineVariable :: Member Runtime r => String -> LispVal -> Eff r ()
defineVariable name val = send (DefineVariable name val)

getFunction :: Member Runtime r => String -> Eff r (Maybe LispFunction)
getFunction name = send (GetFunction name)

currentRuntime :: Member Runtime r => Eff r NameLookup
currentRuntime = send CurrentRuntime

throwError :: Member Runtime r => LispError -> Eff r a
throwError err = send (ThrowError err)

runRuntime :: Eff '[Runtime] a -> Interpreter a
runRuntime (Val x) = return x
runRuntime (E x next) = case extract x of
  Interpret val           -> continueWith val
  GetVariable name        -> do
    nameLookup <- get
    let val = case M.lookup name (names nameLookup) of
          Just (Variable v) -> Just v
          _ -> Nothing
    continueWith val
  GetFunction name        -> do
    nameLookup <- get
    let val = case M.lookup name (names nameLookup) of
          Just (Function f) -> Just f
          _ -> Nothing
    continueWith val
  CurrentRuntime          -> get >>= continueWith
  SetVariable name val    -> modify (addVariable name val) >> continueWith ()
  DefineVariable name val -> modify (addVariable name val) >> continueWith ()
  ThrowError err          -> E.throwError err
  where
    continueWith = runRuntime . qApp next

addVariable :: String -> LispVal -> NameLookup -> NameLookup
addVariable name val = NameLookup . M.insert name (Variable val) . names