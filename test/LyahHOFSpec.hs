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
    describe "flip'" $ do 
        it "flips the first two arguments of a function (divide))" $ 
            flip' (/) 4 8 `shouldBe` 2
        it "plays nice with zip" $ 
            flip' zip [1,2,3,4,5] "hello" `shouldBe` [('h',1), ('e',2), ('l', 3), ('l', 4), ('o',5)]
        it "can be composed with zipWith" $
            zipWith' (flip' div) [2,2..] [10,8,6,4,2] `shouldBe` [5,4,3,2,1]
    describe "map" $ do 
        it "maps (+3) to a list of numbers" $
            map (+3) [1,5,3,1,6] `shouldBe` [4,8,6,4,9]
        it "maps ++ ! to  alist of strings" $ 
             map (++ "!") ["BIFF", "BANG", "POW"] `shouldBe` ["BIFF!", "BANG!", "POW!"]
        it "can take itself as an argument" $ 
            map (map (^2) ) [[1,2] ,[3,4,5,6], [7,8]] `shouldBe` [ [1,4] , [9,16,25,36], [49,64]]
        it "plays nice with replicate" $ 
           map (replicate 3) [3..6] `shouldBe` [ [3,3,3], [4,4,4], [5,5,5], [6,6,6] ] 
        it "can grab the first element of a list of tuples and make a list" $ 
            map fst [(1,2), (3,5), (6,3), (2,6), (2,5)] `shouldBe` [1,3,6,2,2]
	
	     
