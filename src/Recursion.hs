module Recursion
  ( maximum')
where

maximum' :: (Ord a) => [a] -> a
maximum' []    = error "No empty lists, buddy" 
maximum' [x]   = x 
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs
