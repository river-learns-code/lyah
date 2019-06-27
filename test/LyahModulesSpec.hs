module LyahModulesSpec (spec) 
where 

import Test.Hspec
import LyahModules
import Data.List

spec :: Spec 
spec = do
    describe "numUniques" $ do 
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
