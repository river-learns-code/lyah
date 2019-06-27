module LyahModulesSpec (spec) 
where 

import Test.Hspec
import LyahModules

spec :: Spec 
spec = do
    describe "numUniques" $ do 
      it "tells us how many unique elements there are in a list" $ 
          numUniques [1,4,2,8,8,8,2] `shouldBe` 4
