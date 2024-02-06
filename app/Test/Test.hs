module Test.Test where
import Test.Hspec (hspec)
import Test.Parser.Parser (parserSpec)

runTests :: IO ()
runTests = hspec $ do
    parserSpec