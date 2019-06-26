module LyahHOFSpec (spec) where 

import Test.Hspec
import LyahHOF
import Baby
import Support.FlCompare
import Control.Exception (evaluate)


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
        it "plays nice with lambda functions, also lambdas take multiple parameters" $ 
            zipWith' (\a b -> (a*30 + 3)/b) [5,4,3,2,1] [1,2,3,4,5] `shouldBe` [153.0,61.5,31.0,15.75,6.6]
    describe "flip'" $ do 
        it "flips the first two arguments of a function (divide))" $ 
            flip' (/) 4 8 `shouldBe` 2
        it "plays nice with zip" $ 
            flip' zip [1,2,3,4,5] "hello" `shouldBe` [('h',1), ('e',2), ('l', 3), ('l', 4), ('o',5)]
        it "can be composed with zipWith" $
            zipWith' (flip' div) [2,2..] [10,8,6,4,2] `shouldBe` [5,4,3,2,1]
    describe "map'" $ do 
        it "maps (+3) to a list of numbers" $
            map' (+3) [1,5,3,1,6] `shouldBe` [4,8,6,4,9]
        it "maps ++ ! to  alist of strings" $ 
             map' (++ "!") ["BIFF", "BANG", "POW"] `shouldBe` ["BIFF!", "BANG!", "POW!"]
        it "can take itself as an argument" $ 
            map' (map' (^2) ) [[1,2] ,[3,4,5,6], [7,8]] `shouldBe` [ [1,4] , [9,16,25,36], [49,64]]
        it "plays nice with replicate" $ 
           map' (replicate 3) [3..6] `shouldBe` [ [3,3,3], [4,4,4], [5,5,5], [6,6,6] ] 
        it "can grab the first element of a list of tuples and make a list" $ 
            map' fst [(1,2), (3,5), (6,3), (2,6), (2,5)] `shouldBe` [1,3,6,2,2]
        it "plays nice with lambdas, can be combined with some pattern matching" $
            map' (\(a,b) -> a + b) [(1,2), (3,5), (6,3), (2,6) ,(2,5)] `shouldBe` [3,8,9,8,7]
    describe "recursivelyEmptyList" $ do 
        it "takes a List an inefficiently turns it into an empty list" $ 
            recursivelyEmptyList "Bow!" `shouldBe` []
    describe "filter'" $ do 
        it "when applied to a list of numbers with (>3) , can get all numbers from a list that are greater than a given number" $ 
            filter (>3) [1,5,3,2,1,6,4,3,2,1] `shouldBe` [5,6,4]
        it "can pull every instance of 3 out of a list using ==" $ 
            filter' (==3) [1,2,3,4,5] `shouldBe` [3]
        it "can use predicate functions like even" $ 
            filter' even [1..10] `shouldBe` [2,4..10]
        it "plays nice with let, but can I make it work in hspec?" $ 
            let notNull x = not (null x) 
                in filter' notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]] `shouldBe` [[1,2,3],[3,4,5], [2,2]]
        it "can take the lowercase out of a string, I imagine it's great for sanitation if string weren't slow." $
            filter' (`elem` ['a' .. 'z']) "we DON'T, like, LIKE the LYAH book" `shouldBe` "welikethebook"
        it "can take the uppercase out, too" $ 
            filter' (`elem` ['A' .. 'Z'] ) "we DON'T, like, LIKE the LYAH book" `shouldBe` "DONTLIKELYAH"
      --  it "fuck the guy who wrote lyah, he thinks he's a clever shit" $ 
        --    "the guy" `shouldBe` "smarter" --but he never will be 
    describe "largestDivisible" $ do 
        it "returns the largest number smaller than 100,000 and divisible by 3829" $
            largestDivisible `shouldBe` 99554 
    describe "chainsUnder100WithLength15OrGreater" $
       it "gives the number of chains generated from the numbers in [1..100] that are of length greater than 15" $
           chainsUnder100WithLength15OrGreater `shouldBe` 66
    describe "chain" $ do 
        it "creates a list from a sequence using specific rules, based on even or odd. even numbers are divided by two, odd numbers are multiplied by 3 and then incremented" $
            chain 10 `shouldBe` [10,5,16,8,4,2,1]
        it " works on 30, too" $ 
            chain 30 `shouldBe` [30,15,46,23,70,35,106,53,160,80,40,20,10,5,16,8,4,2,1]
    describe "sum'" $ do 
      it "adds the elements of a list together" $ 
          sum' [3,5,2,1] `shouldBe` 11 
      it "is good on empty lists?" $
          sum' [] `shouldBe` 0
    describe "product'" $ do 
        it "multiplies the elements of a list together" $ 
            product' [1,2,3] `shouldBe` 6
        it "throws an exception on an empty list" $
            evaluate (product' []) `shouldThrow` anyException
    describe "last'" $ do
        it "gets the last element of a list" $
           last' "pee" `shouldBe` 'e'
        it "throws an error on empty list" $
            evaluate (last' []) `shouldThrow` anyException
      --  it "will probably run forever if fed a negative number" $
        --    chain (-1) `shouldBe` [1]
--journey of the zumbinis
