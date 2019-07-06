module LyahModulesSpec (spec) 
where 

import Test.Hspec
import LyahModules
import Data.List
import Control.Exception (evaluate)

spec :: Spec 
spec = do
    describe "numUniques" $  
      it "tells us how many unique elements there are in a list" $ 
          numUniques [1,4,2,8,8,8,2] `shouldBe` 4
    describe "intersperse" $ do 
        it "puts an element of a list between each pair of elements in the list" $ 
            intersperse '.' "MONKEY" `shouldBe` "M.O.N.K.E.Y"
        it "puts numbers between other numbers" $
            intersperse 0 [1..6] `shouldBe` [1,0,2,0,3,0,4,0,5,0,6]
    describe "intercalate" $ do 
        it "takes a lists oflists and flattens the result, putting something in between" $ 
            intercalate " " ["hey", "there","folx"] `shouldBe` "hey there folx"
        it " takes works with lists of numbers" $ 
            intercalate [0,0,0] [[1,2,3],[4,5,6],[7,8,9]] `shouldBe` [1,2,3,0,0,0,4,5,6,0,0,0,7,8,9] 
        it "takes an empty list and just flattens" $
            intercalate "" ["fuck", "the","author","of","lyah"] `shouldBe` "fucktheauthoroflyah"
    describe "transpose" $ do 
        it "transposes lists oflists as if they were matrices" $ 
            transpose [[1,2,3],[4,5,6],[7,8,9]] `shouldBe` [[1,4,7],[2,5,8],[3,6,9]]
        it "transposes non rectangular 'matrices' too" $
            transpose ["hey","there","folx"] `shouldBe` ["htf", "eho", "yel", "rx", "e"] 
        it "can be used to model polynomial addition" $ 
            map sum ( transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]])  `shouldBe` [18,8,6,17]
    describe "concat" $ do 
        it "is the same as intercalate with an empty list" $ 
            concat ["hi","there"] `shouldBe` "hithere"
        it "workswith nums too" $ 
            concat [[3,4,5],[2,3,4],[2,1,1]] `shouldBe` [3,4,5,2,3,4,2,1,1]

    describe "concatMap" $ do
       it "is the same as calling map then concat" $
          concatMap (replicate 2) [1,2,3] `shouldBe` [1,1,2,2,3,3]
    describe "and" $ do 
        it "returns true if all the elements of a list ar true" $ 
            and [True, True] `shouldBe` True
        it "returns false if one of the elements of the list is false" $ 
            and ( map (== 4) [4,4,4,3,4] ) `shouldBe` False
    describe "or" $ do 
        it "returns true if one of the lists of an element is true" $ 
            or (map (==4) [2,3,4,5,6,1] ) `shouldBe` True 
        it "returns Flase if all elements of the list are false" $
            or (map  (>4) [1,2,3] ) `shouldBe` False
    describe "any and all " $ do 
        it "any returns true if any   element of a list satisfies the predicate, like we were doing with and, or, and map" $
            any (==4) [2,3,5,6,1,4] `shouldBe` True
        it "all returns false if at least one element does not satisfy the predicate" $
            all (`elem` ['A'..'Z'])  "HEYYALLwhatsup" `shouldBe` False 
    describe "iterate" $ do 
        it "applies a function repeatedly to its argument and puts the results in a list" $
            take 10 ( iterate (*2) 1) `shouldBe` [1,2,4,8,16,32,64,128, 256, 512]
        it "here's an example with strings" $ 
            take 3 (iterate (++ "haha") "haha") `shouldBe` ["haha","hahahaha","hahahahahaha"]
        it "is the greatest-est, I used it for something!" $
            True `shouldBe` True
    describe "splitAt" $ do  
        it "splits a list into two tuples of the same type, with an index" $ 
            splitAt 3 "heyMan" `shouldBe` ("hey","Man")
        it "will return the whole list paired with an empty list if the index fed is large enough" $
            splitAt 100 "YoBro" `shouldBe` ("YoBro","")
        it "takes negative indexes gracefully" $ 
            splitAt (-3) "YoBro" `shouldBe` ("", "YoBro")
        it "can be used to switch words in strings" $
            (let (a,b) = splitAt 2 "YoBro" in b ++ a) `shouldBe` "BroYo"
    describe "takeWhile" $ do 
        it "make a sublist by taking elements, stopping when the predicate first fails" $ 
            takeWhile (>3) [4,5,6,7,8,9,8,1,7,6,5,4,3,2,10] `shouldBe` [4,5,6,7,8,9,8]
        it "works nice with strings, though if you're doing that in production use production libraries" $
            takeWhile (/= ' ') "This is not a sentence." `shouldBe` "This"
        it "can be used to find the sume of all third powers under 1000" $ 
            sum ( takeWhile (<10090) (map (^3) [1..]) ) `shouldBe` 53361
    describe "dropWhile" $ do 
        it  "same as takewhile, but it drops elements until the predicate is false" $
           dropWhile (/= ' ') "This is a sentence." `shouldBe` " is a sentence."
        it "example of peeking inside a list of tuples" $
            head (dropWhile (\(val,y,m,d) -> val < 1000) stock) `shouldBe` (1001.4, 2008, 9, 4) 
        it "chops off everything off the front of a list less than a value" $
            dropWhile (<3) [1,2,2,2,2,3,4,5,4,3,2,1] `shouldBe` [3,4,5,4,3,2,1]
            
