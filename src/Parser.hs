module Parser 
(
  LispVal(..), LispParser
, parseExpr
) 

where

import Debug.Trace (traceShowId)

import Text.ParserCombinators.Parsec ( char, letter, digit, space, string, anyChar, alphaNum
                                     , many, noneOf, many1, skipMany1, oneOf
                                     , optionMaybe
                                     , Parser
                                     , (<|>), try, sepBy, endBy
                                     )
import Numeric
import Data.Char
import Control.Monad (guard)
-- import System.Environment 
import qualified Data.Vector as V

data LispVal = Atom String
             | Char Char
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float 
             | String String
             | Bool Bool
             | Vector (V.Vector LispVal) 
  deriving (Show, Eq)

type LispParser = Parser LispVal

parseExpr :: LispParser
parseExpr = parseString
        <|> try parseBool
        <|> try parseChar
        <|> try parseFloat
        <|> try parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- try $ parseList <|> parseDottedList
               char ')'
               return x
        <|> parseQuasiquote
        <|> try parseVector
        <|> parseAtom

parseString :: LispParser
parseString = do
  char '"'
  x <- many $ parseEscape <|> noneOf "\\\""
  char '"'
  return . String $ x
  where parseEscape = do
          char '\\'
          cLookup <$> anyChar
        cLookup 'n' = '\n'
        cLookup '\\' = '\\'
        cLookup 'r' = '\r'
        cLookup 't' = '\t'
        -- cLookup '"' = '"'
        cLookup x = x

parseChar :: LispParser
parseChar = do
  string "#\\"
  c <- anyChar
  if isAlphaNum c 
  then do
    rest <- optionMaybe $ many1 alphaNum
    return . Char . maybe c cLookup $ rest 
  else return $ Char c
  where cLookup "space" = ' '
        cLookup "newline" = '\n' 


parseAtom :: LispParser
parseAtom = do
  first <- letter <|> symbol
  rest <- many $ letter <|> digit <|> symbol
  return $ Atom (first : rest)
  
parseBool :: LispParser
parseBool = do
  char '#'
  b <- oneOf "tf"
  return . Bool $ case b of
             't' -> True
             'f' -> False

parseNumber :: LispParser
parseNumber = parseDec1
          <|> try parseDec2
          <|> try parseBin
          <|> try parseOctal
          <|> try parseHex
  where parseDec1 = do
          x <- optionMaybe $ char '-'
          y <- many1 digit
          return . Number $ case x of 
                              Just _ -> negate (read y)
                              Nothing -> read y
        parseDec2 = do 
          string "#d"
          x <- many1 digit
          return . Number . read $ x

        parseSet :: String -> String -> (Integer -> Char -> Integer) -> LispParser
        parseSet k range readBase = do 
          string k
          x <- many1 (oneOf range) 
          return $ traceShowId x
          return $ Number . foldl readBase 0 $ x
        parseBin = parseSet "#b" "01" readBin 
        parseOctal = parseSet "#o" "01234567" readOct
        parseHex = parseSet "#x" "0123456789abcdefABCDEF" readHex
        readBin acc c = 2*acc + case c of '1' -> 1
                                          '0' -> 0
        readOct acc c = 8*acc + toInteger (digitToInt c)
        readHex acc c 
          | c `elem` "0123456789" = 16*acc + toInteger (digitToInt c) 
          | otherwise = 16*acc + case c of
                                   'a' -> 10
                                   'b' -> 11
                                   'c' -> 12
                                   'd' -> 13
                                   'e' -> 14
                                   'f' -> 15

parseFloat :: LispParser
parseFloat = parseDecimal
         <|> parseExponent
  where parseDecimal = do
          neg <- optionMaybe $ char '-'
          x <- many1 digit
          char '.'
          y <- many1 digit
          let abs = read $ x ++ "." ++ y
          return . Float $ case neg of
                             Just _ -> negate abs
                             Nothing -> abs
        parseExponent = do
          Float x <- parseDecimal 
          char 'e'
          y <- many1 digit
          return . Float $ x * 10 ^ (read y)
  

parseList :: LispParser
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: LispParser
parseDottedList = do
  head <- endBy parseExpr spaces
  char '.'
  spaces
  tail <- parseExpr
  return $ DottedList head tail

parseQuoted :: LispParser
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiquote :: LispParser
parseQuasiquote = do
  string "`("
  xs <- sepBy (try parseQuoteSplice <|> parseUnquote <|> parseExpr) spaces
  char ')'
  return $ List [Atom "quasiquote", List xs]
  where parseUnquote = do
          char ','
          e <- parseExpr
          return $ List [Atom "unquote", e]
        parseQuoteSplice = do
          string ",@"
          e <- parseExpr
          return $ List [Atom "unquote-splicing", e]


parseVector :: LispParser
parseVector = do
  string "#("
  (List x) <- parseList
  char ')'
  return $ Vector $ V.fromList x

  
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

