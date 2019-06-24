module LyahHOF (
 multThree
 ,multTwoWithNine
 ,multWithEighteen
 ,compareWithHundred
 ,divideByTen
 ,isUpperAlphanum
 ,applyTwice) 
where

multThree :: (Num a) => a -> a -> a -> a 
multThree x y z = x * y * z 

multTwoWithNine  = multThree 9  

multWithEighteen = multTwoWithNine 2

compareWithHundred :: (Ord a, Num a) => a -> Ordering
compareWithHundred  = compare 100 

divideByTen :: (Floating a) => a -> a 
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A' .. 'Z'])

applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x)  
