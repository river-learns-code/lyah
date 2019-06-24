module LyahHOFSpec (spec) where 

import Test.Hspec
import LyahHOF
import Support.FlCompare

spec :: Spec

spec = do 
    describe "multThree" $ do 
        it "multiplies three numbers" $ 
            2 * (-4.2) * 7 `shouldSatisfy` (defAppEq (-58.8))
    describe "defAppEq" $ do 
        it "compares two numbers just fine" $
            defAppEq 3.2 3.5 `shouldBe` False 
    describe "multTwoWithNine" $ do
        it "multiplies two numbers together and multipilies with 9" $
				multTwoWithNine 2 3 `shouldBe` 54
    describe "multWithEighteen" $ do 
        it "multiplies a number with 18" $
            multWithEighteen 10 `shouldBe` 180
    describe "compareWithHundred" $ do 
        it "compares with 100 and returns an ordering" $
            compareWithHundred 35 `shouldBe` GT
    describe "divideByTen" $ do
       it "divides a number by 10" $ 
            divideByTen 30 `shouldBe` 3
    describe "isUpperAlphaNum" $  
        it "says if a character is a capital letter" $
            isUpperAlphanum 'c' `shouldBe` False
    describe "applyTwice" $ do 
       it "applies (+3) twice just fine" $
           applyTwice (+3) 10 `shouldBe` 16
       it "works with string concatenation" $
           applyTwice (++ " HAHA") "HEY" `shouldBe` "HEY HAHA HAHA"
    describe "zipWith'" $ do 
        it "adds lists together if you feed it +" $
            zipWith' (+) [4,2,5,6] [2,6,2,3] `shouldBe` [6,8,7,9]
        it "combines lists of strings together into a nice little [String]" $ 
            zipWith' (++) ["foo ","bar ","baz "] ["fighters", "hoppers", "aldrin"] `shouldBe` ["foo fighters", "bar hoppers", "baz aldrin" ]
        it "can take dynamically generated lists as inputs" $
            zipWith' (*)  (replicate 5 2) [1..] `shouldBe` [2,4..10]  
        it "can take itself as an argument, though that's confusion" $
            zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2], [3,4,5] , [5,4,3]] `shouldBe` [[3,4,6], [9,20,30], [10,12,12]]
        
        
        
        
        
	
	     
