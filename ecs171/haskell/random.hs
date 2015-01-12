module Main where

import AI.HNN.FF.Network
import Numeric.LinearAlgebra

main = do
    n <- createNetwork 2 [2] 1 :: IO (Network Double)
    print $ output n sigmoid (fromList [1, 1])
