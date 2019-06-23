module Recursion
  ( maximum'
   ,replicate'
   ,take')
where

maximum' :: (Ord a) => [a] -> a
maximum' []    = error "No empty lists, buddy" 
maximum' [x]   = x 
maximum' (x:xs) = max x (maximum' xs) 

replicate' ::  (Num i, Ord i) => i -> a -> [a]
replicate' n x 
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x

take' :: Int -> [a] -> [a]  
take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs  
