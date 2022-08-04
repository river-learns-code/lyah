module Baby.BabySpec (spec) where
 
import Test.Hspec
import Baby
import Control.Exception (evaluate) 
import Test.Hspec

delta = 0.001

numAppEq :: (Double, Double) -> (Double, Double) -> Bool
numAppEq (x1, y1) (x2, y2)  = (abs (x1 - x2) < delta) && (abs (y1-y2) < delta ) 
-- do this right later

spec :: Spec
spec = do  
    describe "doubleSmallNumber" $ do
      it "Doubles a number that is smaller than 100."  $
        doubleSmallNumber 13 `shouldBe` 26
      
      it "leaves numbers larger than 100 as is" $
        doubleSmallNumber 244 `shouldBe` 244
        
      it "doubles 100 to 200" $
          doubleSmallNumber 100 `shouldBe` 200 

    describe "doubleSmallNumberSucc" $ do 
      it "doubles numbers less than 100 and than adds 1" $
          doubleSmallNumberSucc 13 `shouldBe` 27
      it "only increments numbers larger than 100 by one." $ 
        doubleSmallNumberSucc 244 `shouldBe` 245
      it "works on floats less than 100"  $
        doubleSmallNumber 0.1 `shouldBe` 0.2
      it "works on floats greater than 100" $
        doubleSmallNumberSucc 101.010 `shouldBe` 102.010 

    describe "lucky" $ do 
        it "RETURNS STRING 'LUCKY NUMBER SEVEN!' if called with 7" $
            lucky 7 `shouldBe` "LUCKYNUMBERSEVEN!"
        it "gives a string saying 'but are you seven? You are not seven' if called with not 7" $
            lucky 22 `shouldBe` "but are you seven? You are not seven" 
        it "should throw some sort of exception if you feed it a float" $
            pendingWith "is this even possible to test?"
            --evaluate (lucky (3.14 :: [RealFloat]) ) `shouldThrow` anyErrorCall
            --latest attempt below
            --evaluate (lucky (3.14 :: Double)) `shouldThrow` anyErrorCall
    describe "rQuickSort" $ do 
        it "does nothing to any already sorted list" $
            rquickSort [1,2,3] `shouldBe` [1,2,3]
        it  "returns [1,2,4,4,5] when given [5,4,1,4,2]" $
            rquickSort [5,4,1,4,2] `shouldBe` [1,2,4,4,5]
        it "sorts 'prof' into 'fopr'" $
           rquickSort "prof" `shouldBe` "fopr"
           
    describe "sayMe" $ do
        it "transmogrifies 1 to \"Uno!\"" $
            sayMe 1 `shouldBe` "Uno!" 
        it "transogrifies 2 to \"Dos!\"" $
            sayMe 2 `shouldBe` "Dos!" 
        it "transmogrifies 3 to \"Tres!\"" $
            sayMe 3 `shouldBe` "Tres!" 
        it "transmogrifies 4 to \"Cuatro!\"" $
            sayMe 4 `shouldBe` "Cuatro!" 
        it "transmogrifies 5 to \"Cinco!\"" $
		    sayMe 5 `shouldBe` "Cinco!"
        it "transmogrifies any other Integral number to a standard message" $
            sayMe (-3) `shouldBe` "Not between 1 and 5" 
     
    describe "factorial" $ do 
        it "factorial of zero is 1" $ 
            factorial 0 `shouldBe` 1
        it "returns 479001600 for the factorial of 12" $
            factorial 12 `shouldBe` 479001600
        it "should throw some sort of error if fed a negative number" $
            evaluate (factorial (-1)) `shouldThrow` anyErrorCall
        
    describe "addVectors" $ do 
        
        it "works with a pair of int two-tuples" $
            (2,3) `addVectors` (3,4) `shouldBe` (5,7)
        it "works with a pair of float two-tuples" $  
                  (2.5, 3.55) `addVectors` (3.14, 6.28)  `shouldSatisfy`  (numAppEq (5.64, 9.83) )
    
    describe "first" $ do 
        it "grabs 'a' from ('a','b', 3)" $
            first ('a','b', 3) `shouldBe` 'a'
    
    describe "second" $ do 
        it "grabs 'b' from ('a','b', 3)" $ 
            second ('a', 'b', 3) `shouldBe` 'b'
    
    describe "third" $ do 
        it "grabs 3 from ('a','b', 3)" $ 
            third ('a','b', 3) `shouldBe` 3
    
    describe "head'" $ do
       it "throws an error on an empty list" $
           head' [] `shouldThrow` anyErrorCall
       it "turns [4.2, 42, 342] into 4.2" $
           head' [4.2,42,342] `shouldBe` 4.2
       it "turns \"Hello\" into 'H'" $
           head' "Hello" `shouldBe` 'H'

    describe "tell" $ do
        it "describes empty lists" $
            tell ([] :: [Int]) `shouldBe` "The list is empty" 
        it "says \"The list has one element: 2\" when fed [2]" $
            tell [2] `shouldBe` "The list has one element: 2"
        it "describes a list with two elements, including both elements" $
           tell "ha" `shouldBe` "The list has two elements: 'h' and 'a'"
        it "describes a list with more than two elements as long, showing the first two elements" $
            tell [3,1,4,1,5,9] `shouldBe` "This list is long. The first two elements are: 3 and 1"

    describe "length'" $ do
        it "returns 0 for an empty list" $
           length' "" `shouldBe` 0
        it "returns the length of a nonempty list" $ 
            length' "happy" `shouldBe` 5
    
    describe "max'" $ do
        it "returns the larger of two integers" $
            max' 3 5 `shouldBe` 5 
        it "also returns the larger of two chars" $
            max' 'c' 'd' `shouldBe` 'd'
    
    describe "myCompare" $ do
        it "returns GT when the first argument is greater" $ 
            3 `myCompare` 2 `shouldBe` GT
        it "returns LT when the second argument is greater" $ 
            "apple" `myCompare` "zeta448" `shouldBe` LT
        it "returns EQ when the arguments are the same" $
            23.433 `myCompare` 23.433 `shouldBe` EQ

    describe "initals" $ do 
        it "gets the first initials from a first name nad last name string" $
            initials "R" "Espinosa" `shouldBe` "R. E."
        it "does not sanitize input or check for capitals, and will happily grab a number if that starts your strings" $
            initials "314158" "bob" `shouldBe` "3. b."        

    describe "cylinder" $ do
        it "returns the surface area of a cylinder" $ 
            cylinder 3 5    `shouldBe` 150.79644737231007  
        it "does not check for them negative numbers" $
            let target = -37.699
                actual = cylinder (-3) 5 
            in abs (target - actual)  `shouldSatisfy` (< delta)

    describe "head'" $ do 
        it "returns  the first element of a list" $ 
            head' "bob" `shouldBe` 'b'
        it "throws an error on an empty list" $
            head' [] `shouldThrow` anyErrorCall

    describe "describeList" $ do    
        it "describes empty lists" $
            describeList [] `shouldBe` "The list is empty."
        it "describes a singleton list" $
            describeList [2] `shouldBe` "The list is a singleton list." 
        it "describes all lists with more than one element as 'longer'" $ 
            describeList [2.5, 3.0, 8.8] `shouldBe` "The list is a longer list."
            

