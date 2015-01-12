module Rod where

import Data.Foldable (maximum)

cutRod :: [Int] -> Int -> Int
cutRod _ 0 = 0
cutRod xs n =
