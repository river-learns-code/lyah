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
        
