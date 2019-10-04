import           Test.Hspec
import           Test.SchemeInterpreter.Parser (parserTests)
import           Test.SchemeInterpreter.Eval (evalTests)

main :: IO ()
main = hspec
  $ do
    parserTests
    evalTests
