module LyahHOF (
 multThree
 ,multTwoWithNine
 ,multWithEighteen
 ,compareWithHundred) 
where

multThree :: (Num a) => a -> a -> a -> a 
multThree x y z = x * y * z 

multTwoWithNine  = multThree 9  

multWithEighteen = multTwoWithNine 2

compareWithHundred :: (Ord a, Num a) => a -> Ordering
compareWithHundred  = compare 100 
