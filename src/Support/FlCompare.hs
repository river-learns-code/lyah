module Support.FlCompare 
(numAppEq
 ,defAppEq)
where


numAppEq :: (Fractional a, Ord a) => a -> a -> a -> Bool 
numAppEq num1 num2 delta = (abs (num1 - num2) < delta)
defaultDelta = 0.001


defAppEq num1 num2 = numAppEq num1 num2 defaultDelta 
