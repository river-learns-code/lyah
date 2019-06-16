module BabySpec where
 
import Test.Hspec
import Baby
import Control.Exception (evaluate) 

main :: IO ()
main = hspec $ do 
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
        it "gives a string saying 'fuck seven anyways' if called with not 7" $
            lucky 22 `shouldBe` "fuck seven anyways" 
        it "should throw some sort of exception if you feed it a float" $
            pendingWith "is this even possible?" 

    describe "jQuickSort" $ do 
        it "returns an empty list if fed an empty list" $ 
            jquickSort [1,2,3] `shouldBe` [1,2,3]
        it  "returns [1,2,4,4,5] when given [5,4,1,4,2]" $
            jquickSort [5,4,1,4,2] `shouldBe` [1,2,4,4,5]
        it "sorts 'prof' into 'fopr'" $
           jquickSort "prof" `shouldBe` "fopr"
           
    describe "sayMe" $ do
        it "transmogrifies 1 to \"Uno!\"" $
            sayMe 1 `shouldBe` "Uno!" 
        it "transogrifies 1 to \"Dos!\"" $
            sayMe 2 `shouldBe` "Dos!" 
        it "transmogrifies 3 to \"Tres!\"" $
            sayMe 3 `shouldBe` "Tres!" 
        it "transmogrifies 4 to \"Cuatro!\"" $
            sayMe 4 `shouldBe` "Cuatro!" 
        it "transmogrifies 5 to \"Cinco!\"" $
		    sayMe 5 `shouldBe` "Cinco!"
        it "transmogrifies any other number to a standard message" $
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
            (2.5, 3.55) `addVectors` (3.14, 6.28) `shouldBe` (5.64, 9.83)

    describe "first" $ do 
        it "grabs 'a' from ('a','b', 3)" $
            first ('a','b', 3) `shouldBe` 'a'
    describe "second" $ do 
        it "grabs 'b' from ('a','b', .)" $ 
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
        
           


