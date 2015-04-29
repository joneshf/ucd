module P2.Morphism (incPairsDists, nearestNeighbors, nearestDistance, tourDistance) where

import Control.Lens ((^.))

import Data.Foldable (minimumBy)
import Data.Int (Int32)
import Data.List (delete)
import Data.Ord (comparing)
import Data.Tuple (swap)

import P2.Distance (Distance(..))
import P2.Node (Node(..), number)

import qualified Data.Map as M

-- We construct a tour by pairing up each node with its immediate successor,
-- and wrapping the end of the list around.

tour :: [a] -> [(a, a)]
tour = zip <*> (uncurry (++) . swap . splitAt 1)

tourDistance :: (Distance s) => [Node s] -> Int32
tourDistance = sum . fmap (uncurry distance) . tour

-- We want to be able to make increasing pairs of nodes.
incPairs :: [Node s] -> [(Node s, Node s)]
incPairs ns = [(i, j) | i <- ns, j <- ns, i^.number < j^.number]

incPairsDists :: (Distance s) => [Node s] -> [(Int, Int, Int32)]
incPairsDists = fmap go . incPairs
    where go (i, j) = (i^.number, j^.number, distance i j)

type Distances = M.Map (Int, Int) Int32

data Nearest a = Nearest
    { visited   :: [a]
    , unvisited :: [a]
    }

mkDistances :: Distance s => [Node s] -> Distances
mkDistances = M.fromList . fmap (\(i, j, d) -> ((i, j), d)) . incPairsDists

mkNearest :: [a] -> Nearest a
mkNearest = uncurry Nearest . splitAt 1

nearestNeighbors :: Distance s => [Node s] -> [Node s]
nearestNeighbors ns = go vs us
    where
    go :: Distance s => [Node s] -> [Node s] -> [Node s]
    go vs             [] = reverse vs
    go vs@(Node current _ _:_) us =
        go (fst (shortest current us):vs) (snd $ shortest current us)
    shortest :: Int -> [Node s] -> (Node s, [Node s])
    shortest i us = maybe undefined (\(n, _) -> (n, delete n us)) $ minimumBy (comparing snd) <$> sequence $ do
        n@(Node j _ _) <- us
        let pair = if i < j then (i, j) else (j, i)
        pure $ (,) n <$> M.lookup pair distances
    Nearest vs us = mkNearest ns
    distances = mkDistances ns

nearestDistance :: Distance s => [Node s] -> Int32
nearestDistance = tourDistance . nearestNeighbors

-- 1. start on an arbitrary vertex as current vertex.
-- 2. find out the shortest edge connecting current vertex and an unvisited vertex V.
-- 3. set current vertex to V.
-- 4. mark V as visited.
-- 5. if all the vertices in domain are visited, then terminate.
-- 6. Go to step 2.
