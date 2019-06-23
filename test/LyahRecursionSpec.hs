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

    describe "replicate'" $ do 
        it "returns an empty list if asked to replicate an number 0 times" $
            replicate' 0 5.5 `shouldBe` []
        it "actually takes 5.5 as an input where int should be?" $
            replicate' 5.5 9.9 `shouldBe` [9.9,9.9,9.9,9.9,9.9,9.9]
    
    describe "take'" $ do 
        it "returns an empty list when you try to take something from an empty list" $ 
            take' 3 []  `shouldBe` ([] :: [Int])
        it "returns the first 3 elements of a String" $
           take' 3 "GHC's errors are confusing" `shouldBe` "GHC"
        
