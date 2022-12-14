module LyahHOF (
 multThree
 ,multTwoWithNine
 ,multWithEighteen
 ,compareWithHundred
 ,divideByTen
 ,isUpperAlphanum
 ,applyTwice
 ,zipWith'
 ,flip'
 ,recursivelyEmptyList
 ,chainsUnder100WithLength15OrGreater
 ,chain
 ,sum'
 ,map'
 ,product'
 ,last'
 ,filter'
 ,sqrtSums
 ,oddSquareSum)
where

multThree :: (Num a) => a -> a -> a -> a 
multThree x y z = x * y * z 

multTwoWithNine :: Integer -> Integer -> Integer
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

zipWith' :: (a->b->c) -> [a]->[b]->[c]
zipWith' _ [] _  = [] 
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = \x y -> f y x 

recursivelyEmptyList :: [a] -> [a]
recursivelyEmptyList [] = []
recursivelyEmptyList (x:xs) = recursivelyEmptyList xs

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n 
   | even n = n:chain (n `div` 2) 
   | odd  n = n:chain (n*3 + 1) 


chainsUnder100WithLength15OrGreater :: Int
chainsUnder100WithLength15OrGreater = length ( filter (\xs -> length xs > 15) (map chain [1..100]))
    
sum' :: (Num a) => [a] -> a 
sum' = foldl (+) 0 

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

product' :: (Num a) => [a] -> a 
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a]-> [a]
filter' predicate = foldr (\x acc ->  if (predicate x) then x : acc else acc) []

last' :: [a] -> a
last' = foldl1 (\_ x -> x) 

sqrtSums :: Int 
sqrtSums = length (takeWhile (<1000)  (scanl1 (+) (map sqrt [1..]))) +1

oddSquareSum :: Integer
oddSquareSum = 
    let oddSquares = filter odd $ map (^2) [1..] 
        belowLimit = takeWhile(<10000) oddSquares
    in sum belowLimit

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0 