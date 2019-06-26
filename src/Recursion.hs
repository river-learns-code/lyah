module Recursion
  ( maximum'
   ,replicate'
   ,take'
   ,reverse'
   ,repeat'
   ,zip'
   ,elem')
where

maximum' :: (Ord a) => [a] -> a
--maximum' []    = error "No empty lists, buddy" 
--maximum' [x]   = x 
--maximum' (x:xs) = max x (maximum' xs) 
maximum' = foldr1 (\x acc -> if x > acc then x else acc) 
replicate' ::  (Num i, Ord i) => i -> a -> [a]
replicate' n x 
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x

take' :: Int -> [a] -> [a]  
take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs  

reverse' :: [a] -> [a]
--reverse' [] = []
--reverse' (x:xs) = reverse' xs ++ [x]
reverse' = foldl (\acc x -> x : acc) []

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ []           = []
zip' [] _           = []
zip' (x:xs) (y: ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

