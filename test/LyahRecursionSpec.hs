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
    
    describe "reverse'" $ do 
        it "makes my old name backwards" $ 
            reverse' "jason" `shouldBe` "nosaj"

    describe "repeat'" $ do 
        it "take' 3 (repeat' 7) gets us 3 sevens" $
            take' 3 (repeat' 7) `shouldBe` [7,7,7]
    
    describe "zip'" $ do 
        it "takes two lists, makes a list of two-tuples out of them" $ 
            zip' [1,2,3] ['a','b','c'] `shouldBe` [(1,'a'), (2,'b'), (3,'c')]
        it "works with lists of different sizes" $
            zip' [3,4] "bob" `shouldBe` [(3,'b'), (4,'o')]
    
    describe "elem'" $ do 
        it "says if something is in a list" $ 
           4 `elem'` [5,4] `shouldBe` True 
        it "says if something is not in a list" $ 
            3 `elem'` [5,4] `shouldBe` False
        it "should always return false on an empty list" $ 
            27 `elem` [] `shouldBe` False
