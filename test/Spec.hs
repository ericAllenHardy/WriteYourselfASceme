{-# LANGUAGE ScopedTypeVariables #-}

import Test.Hspec
import Test.QuickCheck

import Numeric
import Data.Char
import Text.ParserCombinators.Parsec (parse)
import Text.ParserCombinators.Parsec.Error (errorMessages, messageString)
import Data.List (intercalate)
import qualified Data.Vector as V
import Text.Printf

import Parser

readExpr :: String -> LispVal
readExpr input = 
  case parse parseExpr "lisp" input of
    Left err -> String . intercalate ": " . map messageString $ errorMessages err
    Right val -> val

parserTests :: IO ()
parserTests = hspec $ do 
  describe "Parser.parseString" $ do
    it "parses special characters" $ 
      property $ \x -> readExpr (show x) == String x

  describe "Parser.parseChar" $ do
    it "parses special characters too" $ 
      property $ \x -> readExpr ("#\\" ++ [x]) == Char x

  describe "Parser.parseFloat" $ do
    it "parses floats" $
      property $ \x -> readExpr (printf "%.12f" x) == Float x

  describe "Parser.parseNumber" $ do
    it "parses hex" $ 
      property $ let f (NonNegative x) = 
                       readExpr ("#x" ++ showHex x "") == Number x
                 in f

    it "parses octal" $ 
      property $ let f (NonNegative x) =
                       readExpr ("#o" ++ showOct x "") == Number x
                 in f

    it "parses binary" $ 
      property $ let f (NonNegative x) =
                       readExpr ("#b" ++ showBin x "") == Number x 
                 in f

  describe "Parser.parseList" $ do
    it "quotes things" $
      readExpr "('a 2 '(+ 1 2))"
      `shouldBe`
      List [ List [Atom "quote", Atom "a"]
           , Number 2, List [Atom "quote", List [Atom "+", Number 1, Number 2]]]

  describe "Parser.parseQuasiquote" $ do
    it "returns a quasiquote function expr" $ 
      (readExpr "`(list ,(+ 1 2) 4)") 
      `shouldBe` 
      List [ Atom "quasiquote"
           , List [ Atom "list"
                  , List [ Atom "unquote"
                         , List [Atom "+", Number 1, Number 2 ]
                         ]
                  , Number 4
                  ]
           ]
    it "handles array inserts" $ 
      (readExpr "`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)")
      `shouldBe`
      List [ Atom "quasiquote"
           , List [ Atom "a"
                  , List [ Atom "unquote"
                         , List [Atom "+", Number 1, Number 2]
                         ]
                  , List [ Atom "unquote-splicing"
                         , List [ Atom "map", Atom "abs"
                                , List [Atom "quote"
                                       , List [Number 4, Number (-5), Number 6] 
                                       ]
                                ]
                         ]
                  , Atom "b"
                  ]
           ]

  describe "Parser.parseVector" $ do
    it "returns a vector" $ 
      (readExpr "#(1 2 3 4 5)")
      `shouldBe`
      (Vector . V.fromList . map Number $ [1 .. 5])
  where showBin 0 acc = "0"
        showBin 1 acc = '1' : acc
        showBin n acc = showBin (n `div` 2)  $ (intToDigit . fromInteger $ n `mod` 2) : acc

main :: IO ()
main = do
  parserTests
