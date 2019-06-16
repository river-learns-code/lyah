module Baby
 (doubleSmallNumber
 , doubleSmallNumberSucc
 , boomBangs
 , removeNonUppercase
 , rightTriangles
 , rightTriPer24
 , jquickSort
 , factorial
 , circumference
 , lucky
 , sayMe
 , addVectors
 , first
 , second
 , third
 , head'
) where

doubleSmallNumber x     = if x > 100
						  then x 
						  else x*2

doubleSmallNumberSucc x = (if x > 100 then x else x*2) + 1

boomBangs xs            = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]   

removeNonUppercase st   = [c | c <- st, c `elem` ['A'..'Z'] ]

rightTriangles          = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10], a^2+ b^2 ==c^2 ]

rightTriPer24           = [ (a,b,c) | c <- [1..10], b <- [1.110], a <- [1..10], 
                                       a^2 + b^2 == c^2, a+b+c==24 ]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z  

circumference :: Float -> Float
circumference r = 2 * pi * r

jquickSort :: Ord a => [a] -> [a]
-- Using list comprehensions
jquickSort []     = []                               -- The empty list is already sorted
jquickSort (x:xs) = jquickSort [a | a <- xs, a < x]   -- Sort the left part of the list
                   ++ [x] ++                        -- Insert pivot between two sorted parts
                   jquickSort [a | a <- xs, a >= x]  -- Sort the right part of the list

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKYNUMBERSEVEN!"
lucky x = "fuck seven anyways"

sayMe :: (Integral a) => a -> String
sayMe 1 = "Uno!"
sayMe 2 = "Dos!"
sayMe 3 = "Tres!"
sayMe 4 = "Cuatro!"
sayMe 5 = "Cinco!"
sayMe x = "Not between 1 and 5"

factorial :: (Integral a) => a -> a 
factorial 0 = 1
factorial n | n < 0 = error "natural numbers only"
factorial n = n * factorial (n -1) 

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a) 
addVectors (x1, y1) (x2, y2) = (x1+ x2, y1 + y2) 

first :: (a,b,c) -> a 
first(x, _, _) = x
 
second :: (a,b,c) -> b
second (_, y, _) = y

third :: (a,b,c) -> c
third (_, _, z) = z 

head' :: [a] -> a
head' = undefined
