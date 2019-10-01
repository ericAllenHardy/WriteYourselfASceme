{-# LANGUAGE ScopedTypeVariables #-}
{-# HLINT ignore "Redundant do" #-}

import           Test.Hspec
import           Test.QuickCheck

import           Numeric
import           Data.Char
import           Text.ParserCombinators.Parsec  ( parse )
import           Text.ParserCombinators.Parsec.Error
                                                ( errorMessages
                                                , messageString
                                                )
import           Data.List                      ( intercalate )
import qualified Data.Vector                   as V
import           Text.Printf

import           SchemeInterpreter.Parser      as P

readExpr :: String -> LispVal
readExpr input = case parse P.parseExpr "lisp" input of
  Left  err -> String . intercalate ": " . map messageString $ errorMessages err
  Right val -> val

parserTests :: IO ()
parserTests = hspec $ do
  describe "Parser.parseString" $ it "parses special characters" $ do
    property $ \x -> P.readExpr (show x) == return (String x)

  describe "Parser.parseChar" $ it "parses special characters too" $ do
    property $ \x -> P.readExpr ("#\\" ++ [x]) == return (Char x)

  describe "Parser.parseFloat" $ it "parses floats" $ do
    property $ \x -> P.readExpr (printf "%.12f" x) == return (Float x)

  describe "Parser.parseNumber" $ do
    it "parses hex" $ do
      property
        $ let f (NonNegative x) =
                P.readExpr ("#x" ++ showHex x "") == return (Number x)
          in  f

    it "parses octal" $ do
      property
        $ let f (NonNegative x) =
                P.readExpr ("#o" ++ showOct x "") == return (Number x)
          in  f

    it "parses binary" $ do
      property
        $ let f (NonNegative x) =
                P.readExpr ("#b" ++ showBin x "") == return (Number x)
          in  f

  describe "Parser.parseList"
    $          it "quotes things"
    $          P.readExpr "('a 2 '(+ 1 2))"
    `shouldBe` return
                 (List
                   [ List [Atom "quote", Atom "a"]
                   , Number 2
                   , List [Atom "quote", List [Atom "+", Number 1, Number 2]]
                   ]
                 )

  describe "Parser.parseQuasiquote" $ do
    it "returns a quasiquote function expr"
      $          P.readExpr "`(list ,(+ 1 2) 4)"
      `shouldBe` return
                   (List
                     [ Atom "quasiquote"
                     , List
                       [ Atom "list"
                       , List
                         [Atom "unquote", List [Atom "+", Number 1, Number 2]]
                       , Number 4
                       ]
                     ]
                   )
    it "handles array inserts"
      $          P.readExpr "`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)"
      `shouldBe` return
                   (List
                     [ Atom "quasiquote"
                     , List
                       [ Atom "a"
                       , List
                         [Atom "unquote", List [Atom "+", Number 1, Number 2]]
                       , List
                         [ Atom "unquote-splicing"
                         , List
                           [ Atom "map"
                           , Atom "abs"
                           , List
                             [ Atom "quote"
                             , List [Number 4, Number (-5), Number 6]
                             ]
                           ]
                         ]
                       , Atom "b"
                       ]
                     ]
                   )

  describe "Parser.parseVector"
    $          it "returns a vector"
    $          P.readExpr "#(1 2 3 4 5)"
    `shouldBe` return (Vector . V.fromList . map Number $ [1 .. 5])
 where
  showBin 0 acc = "0"
  showBin 1 acc = '1' : acc
  showBin n acc =
    showBin (n `div` 2) $ (intToDigit . fromInteger $ n `mod` 2) : acc

main :: IO ()
main = parserTests
