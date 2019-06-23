module LyahRecursionSpec (spec) where 

import Test.Hspec
import Recursion
import Control.Exception (evaluate)

spec :: Spec

spec = do
    describe "maximum'" $ do
        it "returns an error on an empty List" $
            evaluate (maximum' ([] :: [Int]) ) `shouldThrow` anyErrorCall 
        it "returns the only member of a singleton" $
            maximum' [3] `shouldBe` 3
        it "returns the maximum of a list" $
            maximum' [3.2, 4.8, 40.0] `shouldBe` 40.0
