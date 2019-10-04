{-# HLINT ignore "Redundant do" #-}
module Test.SchemeInterpreter.Parser (parserTests) where

import           Test.Hspec
import           Test.QuickCheck
import           Numeric
import           Data.Char
import           Text.ParserCombinators.Parsec (parse)
import qualified Data.Vector as V
import           Text.Printf
import           SchemeInterpreter.LispVal (LispVal(..))
import qualified SchemeInterpreter.Parser as P

readExpr :: String -> Either String LispVal
readExpr x = case parse P.parseExpr "test" x of
  Left p  -> Left (show p)
  Right r -> Right r

parsesTo :: (a -> String) -> (a -> LispVal) -> a -> Bool
parsesTo toString constuctor x = readExpr (toString x) == return (constuctor x)

parserTests :: SpecWith ()
parserTests = do
  describe "Parser.parseString"
    $ it "parses special characters"
    $ property (show `parsesTo` String)
  describe "Parser.parseChar"
    $ it "parses special characters too"
    $ property ((\x -> "#\\" ++ [x]) `parsesTo` Char)
  describe "Parser.parseFloat"
    $ it "parses floats"
    $ property (printf "%.12f" `parsesTo` Float)
  describe "Parser.parseNumber"
    $ do
      it "parses hex"
        $ do
          property
            $ let f (NonNegative x) = readExpr ("#x" ++ showHex x "")
                    == return (Number x)
              in f
      it "parses octal"
        $ do
          property
            $ let f (NonNegative x) = readExpr ("#o" ++ showOct x "")
                    == return (Number x)
              in f
      it "parses binary"
        $ do
          property
            $ let f (NonNegative x) = readExpr ("#b" ++ showBin x "")
                    == return (Number x)
              in f
  describe "Parser.parseList"
    $ it "quotes things"
    $ readExpr "('a 2 '(+ 1 2))"
    `shouldBe` return
      (List
         [ List [Atom "quote", Atom "a"]
         , Number 2
         , List [Atom "quote", List [Atom "+", Number 1, Number 2]]])
  describe "Parser.parseQuasiquote"
    $ do
      it "returns a quasiquote function expr"
        $ readExpr "`(list ,(+ 1 2) 4)"
        `shouldBe` return
          (List
             [ Atom "quasiquote"
             , List
                 [ Atom "list"
                 , List [Atom "unquote", List [Atom "+", Number 1, Number 2]]
                 , Number 4]])
      it "handles array inserts"
        $ readExpr "`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)"
        `shouldBe` return
          (List
             [ Atom "quasiquote"
             , List
                 [ Atom "a"
                 , List [Atom "unquote", List [Atom "+", Number 1, Number 2]]
                 , List
                     [ Atom "unquote-splicing"
                     , List
                         [ Atom "map"
                         , Atom "abs"
                         , List
                             [ Atom "quote"
                             , List [Number 4, Number (-5), Number 6]]]]
                 , Atom "b"]])
  describe "Parser.parseVector"
    $ it "returns a vector"
    $ readExpr "#(1 2 3 4 5)"
    `shouldBe` return (Vector . V.fromList . map Number $ [1 .. 5])
  where
    showBin 0 _ = "0"
    showBin 1 acc = '1':acc
    showBin n acc = showBin (n `div` 2)
      $ (intToDigit . fromInteger $ n `mod` 2):acc