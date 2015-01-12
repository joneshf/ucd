module Main where

import AI.HNN.FF.Network (Samples, (-->), createNetwork, output, quadError, tanh', trainNTimes, trainUntil)
import Data.Foldable (traverse_)
import Debug.Trace (traceShow)
import Numeric.LinearAlgebra (fromList)

samples :: Samples Double
samples = [ fromList [0, 0] --> fromList [0]
          , fromList [0, 1] --> fromList [1]
          , fromList [1, 0] --> fromList [1]
          , fromList [1, 1] --> fromList [0]
          ]

main :: IO ()
main = do
    net <- createNetwork 2 [2] 1
    --traverse_ (print . output net tanh . fst) samples
    let trained = trainUntil (\_ n s -> traceShow (quadError tanh n s) False) 0.8 tanh tanh' net samples
    let smartNet = trainNTimes 1000 0.8 tanh tanh' net samples
    traverse_ (print . output trained tanh . fst) samples
