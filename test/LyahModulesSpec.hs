module LyahModulesSpec
   ( spec
   )
where

import           Control.Exception              ( evaluate )
import           Data.List
import           LyahModules
import           Test.Hspec

spec :: Spec
spec = do
   describe "numUniques"
      $          it "tells us how many unique elements there are in a list"
      $          numUniques [1, 4, 2, 8, 8, 8, 2]
      `shouldBe` 4
   describe "intersperse" $ do
      it "puts an element of a list between each pair of elements in the list"
         $          intersperse '.' "MONKEY"
         `shouldBe` "M.O.N.K.E.Y"
      it "puts numbers between other numbers"
         $          intersperse 0 [1 .. 6]
         `shouldBe` [1, 0, 2, 0, 3, 0, 4, 0, 5, 0, 6]
   describe "intercalate" $ do
      it
            "takes a lists oflists and flattens the result, putting something in between"
         $          intercalate " " ["hey", "there", "folx"]
         `shouldBe` "hey there folx"
      it " takes works with lists of numbers"
         $          intercalate [0, 0, 0] [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
         `shouldBe` [1, 2, 3, 0, 0, 0, 4, 5, 6, 0, 0, 0, 7, 8, 9]
      it "takes an empty list and just flattens"
         $          intercalate "" ["i", "don't", "like", "this", "book"]
         `shouldBe` "idon'tlikethisbook"

   describe "transpose" $ do
      it "transposes lists of lists as if they were matrices"
         $          transpose [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
         `shouldBe` [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
      it "transposes non rectangular 'matrices' too"
         $          transpose ["hey", "there", "folx"]
         `shouldBe` ["htf", "eho", "yel", "rx", "e"]
      it "can be used to model polynomial addition"
         $ map sum (transpose [[0, 3, 5, 9], [10, 0, 0, 9], [8, 5, 1, -1]])
         `shouldBe` [18, 8, 6, 17]
  
   describe "concat" $ do
      it "is the same as intercalate with an empty list"
         $          concat ["hi", "there"]
         `shouldBe` "hithere"
      it "workswith nums too"
         $          concat [[3, 4, 5], [2, 3, 4], [2, 1, 1]]
         `shouldBe` [3, 4, 5, 2, 3, 4, 2, 1, 1]

   describe "concatMap" $ do
      it "is the same as calling map then concat"
         $          concatMap (replicate 2) [1, 2, 3]
         `shouldBe` [1, 1, 2, 2, 3, 3]
      it "concatMap (take 3) [[1..], [10..], [100..], [1000..]] becomes [1,2,3,10,11,12,100,101,102,1000,1001,1002]"$
        concatMap (take 3) [[1..], [10..], [100..], [1000..]] `shouldBe` [1,2,3,10,11,12,100,101,102,1000,1001,1002]

   describe "and" $ do
      it "returns true if all the elements of a list ar true"
         $          and [True, True]
         `shouldBe` True
      it "returns false if one of the elements of the list is false"
         $          and (map (== 4) [4, 4, 4, 3, 4])
         `shouldBe` False
   
   describe "or" $ do
      it "returns true if one of the lists of an element is true"
         $          or (map (== 4) [2, 3, 4, 5, 6, 1])
         `shouldBe` True
      it "returns False if all elements of the list are false"
         $          or (map (> 4) [1, 2, 3])
         `shouldBe` False
   
   describe "any and all " $ do
      it
            "any returns true if any   element of a list satisfies the predicate, like we were doing with and, or, and map"
         $          any (== 4) [2, 3, 5, 6, 1, 4]
         `shouldBe` True
      it
            "all returns false if at least one element does not satisfy the predicate"
         $          all (`elem` ['A' .. 'Z']) "HEYYALLwhatsup"
         `shouldBe` False
   
   describe "iterate" $ do
      it
            "applies a function repeatedly to its argument and puts the results in a list"
         $          take 10 (iterate (* 2) 1)
         `shouldBe` [1, 2, 4, 8, 16, 32, 64, 128, 256, 512]
      it "here's an example with strings"
         $          take 3 (iterate (++ "haha") "haha")
         `shouldBe` ["haha", "hahahaha", "hahahahahaha"]
      it "is the greatest-est, I used it for something!" $ True `shouldBe` True
   
   describe "splitAt" $ do
      it "splits a list into two tuples of the same type, with an index"
         $          splitAt 3 "heyMan"
         `shouldBe` ("hey", "Man")
      it
            "will return the whole list paired with an empty list if the index fed is large enough"
         $          splitAt 100 "YoBro"
         `shouldBe` ("YoBro", "")
      it "takes negative indexes gracefully"
         $          splitAt (-3) "YoBro"
         `shouldBe` ("", "YoBro")
      it "can be used to switch words in strings"
         $          (let (a, b) = splitAt 2 "YoBro" in b ++ a)
         `shouldBe` "BroYo"
   
   describe "takeWhile" $ do
      it
            "make a sublist by taking elements, stopping when the predicate first fails"
         $ takeWhile (> 3) [4, 5, 6, 7, 8, 9, 8, 1, 7, 6, 5, 4, 3, 2, 10]
         `shouldBe` [4, 5, 6, 7, 8, 9, 8]
      it
            "works nice with strings, though if you're doing that in production use production libraries"
         $          takeWhile (/= ' ') "This is not a sentence."
         `shouldBe` "This"
      it "can be used to find the sume of all third powers under 10000"
         $          sum (takeWhile (< 10090) (map (^ 3) [1 ..]))
         `shouldBe` 53361

   describe "dropWhile" $ do
      it "same as takewhile, but it drops elements until the predicate is false"
         $          dropWhile (/= ' ') "This is a sentence."
         `shouldBe` " is a sentence."
      it "example of peeking inside a list of tuples"
         $          head (dropWhile (\(val, y, m, d) -> val < 1000) stock) -- stock is imported
         `shouldBe` (1001.4, 2008, 9, 4)
      it "chops off everything off the front of a list less than a value"
         $          dropWhile (< 3) [1, 2, 2, 2, 2, 3, 4, 5, 4, 3, 2, 1]
         `shouldBe` [3, 4, 5, 4, 3, 2, 1]

   describe "span" $ do
      it "is like takeWhile, returns tuple of list kept and dropped"
         $ let (fw, rest) = span (/= ' ') "This is a sentence."
           in  ("First word :" ++ fw ++ ", the rest:" ++ rest)
                  `shouldBe` "First word :This, the rest: is a sentence."
      it "is basically the opposite of break"
         $          span (/= 4) [1, 2, 3, 4, 5, 6, 7]
         `shouldBe` break (== 4) [1, 2, 3, 4, 5, 6, 7]
  
   describe "break"
      $          it "separates a list into tuples when the predicate is false"
      $          break (== 4) [1, 2, 3, 4, 5, 6, 7]
      `shouldBe` ([1, 2, 3], [4, 5, 6, 7])
  
   describe "sort"
      $          it "does what it says on the tin"
      $          sort [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]
      `shouldBe` [1, 1, 2, 3, 3, 4, 5, 5, 5, 6, 9]
  
   describe "group" $ do
      it "groups elements into sublists if they're equal"
         $          group [1, 2, 2, 2, 1, 1, 4, 3, 2]
         `shouldBe` [[1], [2, 2, 2], [1, 1], [4], [3], [2]]
      it "combines with sort to see how many of each element is in the list"
         $          (map (\l@(x : xs) -> (x, length l)) . group . sort) -- @ is syntactic sugar, look up if necessary 
                       [1, 2, 2, 2, 1, 1, 4, 3, 2]
         `shouldBe` [(1, 3), (2, 4), (3, 1), (4, 1)]
   
   describe "inits and tails" $ do
      it
            "inits is a list of sublists, of increasing size, each starting from the beginning of the list "
         $          inits "w00t"
         `shouldBe` ["", "w", "w0", "w00", "w00t"]
      it "the first element of inits is an empty list"
         $          head (inits "blargl")
         `shouldBe` ""
      
      it "tails of w00t should be [\"\",\"t\", \"0t\", \"00t\", \"w00t\"] " $
           tails "w00t" `shouldBe` ["w00t","00t", "0t", "t",""]

      it
            "they can be combined to make a nice list of tuples where each tuple contains the whole word"
         $ let w = "w00t"
           in  (inits w `zip` tails w)
                  `shouldBe` [ (""    , "w00t")
                             , ("w"   , "00t")
                             , ("w0"  , "0t")
                             , ("w00" , "t")
                             , ("w00t", "")
                             ]
   describe "search" $ do
      it "sees if a string shows up at the inside of another string "
         $          search "bal" "blargl"
         `shouldBe` False
      it
            "turns out to be the same as isInfixOf (perfect quickcheck candidate, btw)"
         $          isInfixOf "cerol" "cerolia"
         `shouldBe` search "cerol" "cerolia"

   describe "isInfixOf" $ do
      it "find out if a string is a substring of a second string"
         $          isInfixOf "cat" "ima cat burglar"
         `shouldBe` True
      it "is case sensitive"
         $          isInfixOf "Cat" "ima cat burglar"
         `shouldBe` False
      it "here's another false example"
         $          isInfixOf "cats" "ima cat burglar"
         `shouldBe` False
   
   describe "isPrefixOf" $ do
      it
            "tells you if a string is at the beginning of another string.(list,really)"
         $          isPrefixOf "hey" "heyMan"
         `shouldBe` True
      it "doesnt'work if the search list is in the middle"
         $          isPrefixOf "hey" "oh hey there"
         `shouldBe` False
   describe "isSuffixOf" $ do
      it "tells you if a list is at the end of another list"
         $          isSuffixOf "there!" "hithere!"
         `shouldBe` True
      it "here is a false example"
         $            "there!"
         `isSuffixOf` "hithere"
         `shouldBe`   False
   describe "partition" $ do
      it "splits a list into a tuple of two lists via a predicate"
         $          partition (`elem` ['A' .. 'Z']) "WEdoLOVEall"
         `shouldBe` ("WELOVE", "doall")
      it "the list satisfying the predicate comes first"
         $          partition (> 3) [1, 3, 5, 6, 3, 2, 1, 0, 3, 7]
         `shouldBe` ([5, 6, 7], [1, 3, 3, 2, 1, 0, 3])
   describe "find" $ do
      it "take a list, predicate, returns first element satifying predicate"
         $          find (> 4) [1, 2, 3, 4, 5, 6]
         `shouldBe` Just 5
      it "returns Nothing when it finds nothing"
         $          find (> 9) [1, 2, 3, 4, 5, 6]
         `shouldBe` Nothing
   describe "elemIndex" $ do
      it "gives the index of a list element as a Maybe Int"
         $           4
         `elemIndex` [1, 2, 3, 4, 5, 6]
         `shouldBe`  Just 3
      it "returns Nothing if the element isn't found"
         $           10
         `elemIndex` [1, 2, 3, 4, 5, 6]
         `shouldBe`  Nothing
   describe "elemIndices" $ do
      it "is much like elemIndex  returns a list of indices"
         $             ' '
         `elemIndices` "where are the spaces?"
         `shouldBe`    [5, 9, 13]
      it "returns an empty list instead of Nothing"
         $             6
         `elemIndices` [3, 8]
         `shouldBe`    []
   describe "findIndex" $ do
      it "returns the index of the first element that satisfies a predicate"
         $          findIndex (== 4) [5, 3, 2, 1, 6, 4]
         `shouldBe` Just 5
      it "returns a Maybe, so nothing if it's not there"
         $          findIndex (> 7) [5, 3, 2, 1, 6, 4]
         `shouldBe` Nothing
   describe "findIndices"
      $ it "returns a list of the indices of elems that satisfy the predicate"
      $ findIndices (`elem` ['A' .. 'Z']) "Where Are The Caps?!"
      `shouldBe` [0, 6, 10, 14]
   describe "lines"
      $          it "takes a string around returns a list of lines"
      $          lines "1st line\n 2nd line\nthird line"
      `shouldBe` ["1st line", " 2nd line", "third line"]
   describe "unlines"
      $ it "takes a list of strings and returns a single multiline string"
      $ unlines ["first", "2nd", "3rd"]
      `shouldBe` "first\n2nd\n3rd\n"

