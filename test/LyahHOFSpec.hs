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
        
        
        
        
        
        
	
	     
