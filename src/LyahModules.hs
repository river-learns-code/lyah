module LyahModules (numUniques)
where 

import Data.List 
import qualified Data.Map as M

numUniques :: (Eq a) => [a] -> Int 
numUniques = length . nub 
