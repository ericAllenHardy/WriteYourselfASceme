{-# LANGUAGE DeriveFunctor #-}

module SchemeInterpreter.LispVal
    ( LispVal(..)
    , LispError(..)
    , FuncApplication(..)) where

import           Control.Monad.Except
import qualified Data.Vector as V
import           Text.ParserCombinators.Parsec (ParseError)
import           Control.Monad.Freer (Member, Eff)

data LispVal = Atom String
             | Char Char
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | String String
             | Bool Bool
             | Vector (V.Vector LispVal)
  deriving (Eq)

instance Show LispVal where
  show (Atom atom) = atom
  show (Char c) = show c
  show (List ls) = "(" ++ showLs ls ++ ")"
  show (DottedList head tail) = "(" ++ showLs head ++ " . " ++ show tail ++ ")"
  show (Number n) = show n
  show (Float x) = show x
  show (String s) = show s
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Vector ls) = "#(" ++ showLs (V.toList ls) ++ ")"

showLs = unwords . map show

data LispError =
    NumArgs Integer [LispVal]
  | NumArgsRange Integer Integer [LispVal]
  | ValueError String LispVal
  | TypeMismatch String LispVal
  | ParserError ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String
  deriving (Eq)

instance Show LispError where
  show (UnboundVar message varname) = message ++ ": " ++ varname
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction message func) = message ++ ": " ++ show func
  show (ValueError msg found) = msg ++ ": " ++ show found
  show (NumArgs expected found) =
    "Expected " ++ show expected ++ " args; found values " ++ showLs found
  show (NumArgsRange min max found) = "Expected between "
    ++ show min
    ++ " and "
    ++ show max
    ++ " args; found values "
    ++ showLs found
  show (TypeMismatch expected found) =
    "Invalid type: expected " ++ expected ++ ", found " ++ show found
  show (ParserError parseErr) = "Parse error at " ++ show parseErr

data FuncApplication a = FAValue a
                       | FAError LispError
  deriving (Functor)

instance Applicative FuncApplication where
  pure = FAValue

  f <*> x = case x of
    FAError e  -> FAError e
    FAValue x' -> case f of
      FAError e  -> FAError e
      FAValue f' -> FAValue (f' x')