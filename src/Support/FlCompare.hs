module Support.FlCompare 
(numAppEq
 ,defAppEq
 ,defListAppEq)
where


numAppEq :: (Num a, Ord a) => a -> a -> a -> Bool 
numAppEq num1 num2 delta = (abs (num1 - num2) < delta)
defaultDelta = 0.001


defAppEq num1 num2 = numAppEq num1 num2 defaultDelta 

listAppEq :: (Num a, Ord a) => [a] -> [a] -> a -> Bool
listAppEq xs ys delta =  not (False `elem` differences  )
    where 
      differences = zipWith (\x y -> numAppEq x y delta) xs ys 
      

defListAppEq xs ys = listAppEq xs ys defaultDelta
