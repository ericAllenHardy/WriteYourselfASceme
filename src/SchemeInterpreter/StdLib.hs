module SchemeInterpreter.StdLib (stdLibFunctions, FuncApplication(..)) where

import qualified Data.Map.Strict as M
import           Text.Read (readMaybe)
import           SchemeInterpreter.LispVal (LispError(..), LispVal(..)
                                          , FuncApplication(..))

stdLibFunctions :: M.Map String ([LispVal] -> FuncApplication LispVal)
stdLibFunctions = M.fromList
  [ ("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("/", numericBinop div)
  , ("mod", numericBinop mod)
  , ("quotient", numericBinop quot)
  , ("remainder", numericBinop rem)
  , ("=", numBoolBinop (==))
  , ("<", numBoolBinop (<))
  , (">", numBoolBinop (>))
  , ("/=", numBoolBinop (/=))
  , (">=", numBoolBinop (>=))
  , ("<=", numBoolBinop (<=))
  , ("&&", boolBoolBinop (&&))
  , ("||", boolBoolBinop (||))
  , ("string=?", strBoolBinop (==))
  , ("string<?", strBoolBinop (<))
  , ("string>?", strBoolBinop (>))
  , ("string<=?", strBoolBinop (<=))
  , ("string>=?", strBoolBinop (>=))
  , ("car", car)
  , ("cdr", cdr)
  , ("cons", cons)
  , ("eq?", eqv)
  , ("eqv?", eqv)
  , ("string?", boolMonop isString)
    -- , ( "make-string", undefined) -- makeString)
    -- , ( "string", undefined)-- stringConcat)
  , ("string-length", stringLength)
  , ("string-ref", stringRef)]

-- , ("string-set!"  , undefined)
numericBinop
  :: (Integer -> Integer -> Integer) -> [LispVal] -> FuncApplication LispVal
numericBinop op args
  | length args /= 2 = FAError (NumArgs 2 args)
  | otherwise = Number . foldl1 op <$> traverse unpackNum args

boolBinop :: (LispVal -> FuncApplication a)
          -> (a -> a -> Bool)
          -> [LispVal]
          -> FuncApplication LispVal
boolBinop unpacker op args
  | length args /= 2 = FAError (NumArgs 2 args)
  | otherwise = Bool <$> (op <$> unpacker (head args) <*> unpacker (args !! 1))

numBoolBinop
  :: (Integer -> Integer -> Bool) -> [LispVal] -> FuncApplication LispVal
numBoolBinop = boolBinop unpackNum

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> FuncApplication LispVal
boolBoolBinop = boolBinop unpackBool

strBoolBinop
  :: (String -> String -> Bool) -> [LispVal] -> FuncApplication LispVal
strBoolBinop = boolBinop unpackStr

unpackNum :: LispVal -> FuncApplication Integer
unpackNum (Number n) = FAValue n
unpackNum (String s) = case readMaybe s :: (Maybe Integer) of
  Just n  -> FAValue n
  Nothing -> FAError (TypeMismatch "integer" (String s))
unpackNum (List [n]) = unpackNum n
unpackNum x = FAError (TypeMismatch "integer" x)

unpackBool :: LispVal -> FuncApplication Bool
unpackBool (Bool b) = FAValue b
unpackBool x = FAError (TypeMismatch "bool" x)

unpackStr :: LispVal -> FuncApplication String
unpackStr (String s) = FAValue s
unpackStr x = FAError (TypeMismatch "string" x)

car :: [LispVal] -> FuncApplication LispVal
car [List (x:_)] = FAValue x
car [DottedList (x:_) _] = FAValue x
car [badArg] = FAError $ TypeMismatch "pair" badArg
car badArgList = FAError $ NumArgs 1 badArgList

cdr :: [LispVal] -> FuncApplication LispVal
cdr [List (_:xs)] = FAValue $ List xs
cdr [DottedList [_] x] = FAValue x
cdr [DottedList (_:xs) x] = FAValue $ DottedList xs x
cdr [badArg] = FAError $ TypeMismatch "pair" badArg
cdr badArgList = FAError $ NumArgs 1 badArgList

cons :: [LispVal] -> FuncApplication LispVal
cons [x1, List []] = FAValue $ List [x1]
cons [x, List xs] = FAValue $ List (x:xs)
cons [x, DottedList xs xlast] = FAValue $ DottedList (x:xs) xlast
cons [x1, x2] = FAValue $ DottedList [x1] x2
cons badArgList = FAError (NumArgs 2 badArgList)

eqv :: [LispVal] -> FuncApplication LispVal
eqv [a, b] = FAValue (Bool $ eqv' a b)
eqv badArgList = FAError $ NumArgs 2 badArgList

eqv' :: LispVal -> LispVal -> Bool
eqv' (Bool a) (Bool b) = a == b
eqv' (Number a) (Number b) = a == b
eqv' (String a) (String b) = a == b
eqv' (Atom a) (Atom b) = a == b
eqv' (DottedList xs x) (DottedList ys y) =
  eqv' (List $ xs ++ [x]) (List $ ys ++ [y])
eqv' (List a) (List b) = length a == length b && all eqvPair (zip a b)
  where
    eqvPair (x1, x2) = eqv' x1 x2
eqv' _ _ = False

boolMonop :: (LispVal -> Bool) -> [LispVal] -> FuncApplication LispVal
boolMonop check [x] = FAValue (Bool $ check x)
boolMonop _ args = FAError (NumArgs 1 args)

isString :: LispVal -> Bool
isString (String _) = True
isString _ = False

{-
makeString :: [LispVal] -> FuncApplication LispVal
makeString [Number k] = FAValue (String $ replicate (fromIntegral k) 'a')
makeString [x] = FAError (TypeMismatch "integer" x)
makeString [Number k, Char c] = FAValue (String $ replicate (fromIntegral k) c)
makeString [x, Char _] = FAError (TypeMismatch "integer" x)
makeString [_, x] = FAError (TypeMismatch "char" x)
makeString args = FAError (NumArgsRange 1 2 args)

stringConcat :: [LispVal] -> FuncApplication LispVal
stringConcat
  xs = either FAError (FAValue . String . reverse) (foldl' go (Right "") xs)
  where
    go (Right ls) (Char c) = Right (c:ls)
    go l @ (Left _) _ = l
    go _ x = Left (TypeMismatch "char" x)
-}
stringLength :: [LispVal] -> FuncApplication LispVal
stringLength [String s] = FAValue (Number . fromIntegral $ length s)
stringLength [arg] = FAError (TypeMismatch "string" arg)
stringLength args = FAError (NumArgs 1 args)

stringRef :: [LispVal] -> FuncApplication LispVal
stringRef [String s, i @ (Number n)] =
  if 0 <= n && n <= fromIntegral (length s)
  then FAValue (Char $ s !! fromIntegral n)
  else FAError (ValueError "invalid string index" i)
stringRef [String _, x] = FAError (TypeMismatch "integer" x)
stringRef [x, _] = FAError (TypeMismatch "string" x)
stringRef args = FAError (NumArgs 2 args)