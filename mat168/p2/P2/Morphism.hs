module P2.Morphism (incPairsDists, tourDistance) where

import Control.Lens ((^.))

import Data.Tuple (swap)

import P2.Distance (Distance(..))
import P2.Node (Node, number)

-- We construct a tour by pairing up each node with its immediate successor,
-- and wrapping the end of the list around.

tour :: [a] -> [(a, a)]
tour = zip <*> (uncurry (++) . swap . splitAt 1)

tourDistance :: (Distance s, Integral a) => [Node s] -> a
tourDistance = sum . fmap (uncurry distance) . tour

-- We want to be able to make increasing pairs of nodes.
incPairs :: [Node s] -> [(Node s, Node s)]
incPairs ns = [(i, j) | i <- ns, j <- ns, i^.number < j^.number]

incPairsDists :: (Distance s, Integral a) => [Node s] -> [(Int, Int, a)]
incPairsDists = fmap go . incPairs
    where go (i, j) = (i^.number, j^.number, distance i j)
