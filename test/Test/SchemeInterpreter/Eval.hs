{-# HLINT ignore "Redundant do" #-}
module Test.SchemeInterpreter.Eval where

import           Test.Hspec
import           SchemeInterpreter.Interpreter (evalLispFile)

reducesTo :: String -> String -> IO ()
reducesTo first next = evalLispFile first `shouldBe` next

evalTests :: SpecWith ()
evalTests = do
  describe "Eval.comparisons"
    $ it "compares >, <, =, >=, <="
    $ do
      "(< 10 15)" `reducesTo` "#t"
      "(> 10 15)" `reducesTo` "#f"
      "(= 10 15)" `reducesTo` "#f"
  describe "Eval.quoted"
    $ it "returns a quoted list as-is"
    $ "'(a 2 '(5 #\\c))" `reducesTo` "(a 2 '(5 #\\c))"
  describe "Eval.if"
    $ it "chooses a clause"
    $ "(if \
        \(> (string-length \"hello\") 3) \
        \1 0)"
    `reducesTo` "1"
  describe "Eval.define"
    $ it "sets a value"
    $ "(define foo 6)\nfoo" `reducesTo` "6"
  describe "Eval.set!"
    $ it "sets a value"
    $ "(define foo 6)\n(set! foo 9)\nfoo" `reducesTo` "9"
