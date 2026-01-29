module My06 where

import Data.List
import qualified Data.Map as M

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

myMap = M.insert 5 'x' M.empty

myMap2 = M.insert 7 'x' (M.fromList [(5, 'a'), (3, 'b')])
