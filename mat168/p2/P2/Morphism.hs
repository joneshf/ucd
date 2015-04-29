{-# LANGUAGE TupleSections #-}
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

-- Some types for the nearest neighbor algorithm.
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
nearestNeighbors ns = go $ mkNearest ns
    where
    -- Our main iteration.
    go :: Distance s => Nearest (Node s) -> [Node s]
    go (Nearest vs                      []) = reverse vs
    go (Nearest vs@(Node current _ _:_) us) =
        let m = shortest current us
        -- Handle `Nothing` even though it better not happen.
        in maybe (go $ Nearest vs []) (\(v, us') -> go $ Nearest (v:vs) us') m
    -- Compute the shortest distance.
    -- If it ever returns `Nothing`, we're in trouble.
    shortest :: Int -> [Node s] -> Maybe (Node s, [Node s])
    shortest i us = do
        ns <- sequence $ do
            n@(Node j _ _) <- us
            let pair = if i < j then (i, j) else (j, i)
            pure $ (n, ) <$> M.lookup pair distances
        let n = fst $ minimumBy (comparing snd) ns
        pure (n, delete n us)
    -- Compute the distances once.
    distances = mkDistances ns

-- Compute the nearest neighbors and then run the tour.
nearestDistance :: Distance s => [Node s] -> Int32
nearestDistance = tourDistance . nearestNeighbors
