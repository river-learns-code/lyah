module LyahModules (numUniques,stock)
where 

import Data.List 
import qualified Data.Map as M

numUniques :: (Eq a) => [a] -> Int 
numUniques = length . nub 

stock :: [(Float,Float,Float,Float)]
stock = [(994.4,2008,9,1), (995.2,2008,9,2),(999.2,2008,9,3),(1001.4, 2008, 9,4), (998.3,2008,9,5)]
