module Support.FlCompare 
(numAppEq
 ,defAppEq
 ,defListAppEq)
where


numAppEq :: (Num a, Ord a) => a -> a -> a -> Bool 
numAppEq delta num1 num2  = (abs (num1 - num2) < delta)
defaultDelta = 0.001


defAppEq num1 num2 = numAppEq defaultDelta num1 num2 

listAppEq :: (Num a, Ord a) => [a] -> [a] -> a -> Bool
listAppEq xs ys delta =  (False `notElem` comparisons)
    where 
      comparisons = zipWith (numAppEq delta) xs ys 
-- could've done this with a fold

defListAppEq xs ys = listAppEq xs ys defaultDelta

