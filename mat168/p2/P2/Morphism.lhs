Here we collect different morphisms on nodes.

> module P2.Morphism ( incPairsDists, nearestNeighbors, nearestDistance, tour
>                    , tourDistance
>                    ) where

> import Control.Lens ((^.))
>
> import Data.Foldable (minimumBy)
> import Data.Int (Int32)
> import Data.List (delete)
> import Data.Ord (comparing)
> import Data.Tuple (swap)
>
> import P2.Distance (Distance(..))
> import P2.Node (Node(..), number)
>
> import qualified Data.Set as S

We construct a tour by
pairing up each node with its immediate successor,
and wrapping the end of the list around.

> tour :: [a] -> [(a, a)]
> tour = zip <*> (uncurry (++) . swap . splitAt 1)
>
> tourDistance :: (Distance s) => [(Node s, Node s)] -> Int32
> tourDistance = sum . fmap (uncurry distance)

We want to be able to make increasing pairs of nodes.

> incPairs :: [Node s] -> [(Node s, Node s)]
> incPairs ns = [(i, j) | i <- ns, j <- ns, i^.number < j^.number]
>
> incPairsDists :: (Distance s) => [Node s] -> [(Int, Int, Int32)]
> incPairsDists = fmap go . incPairs
>     where go (i, j) = (i^.number, j^.number, distance i j)

Compute the nearest neighbors and then run the tour.

> nearestDistance :: Distance s => [Node s] -> Int32
> nearestDistance = tourDistance . tour . nearestNeighbors

Make `nearestNeighbors'` slightly less polymorphic.

> nearestNeighbors :: Distance s => [Node s] -> [Node s]
> nearestNeighbors = nearestNeighbors' distance

Codify the nearest neighbor algorithm as a list of visited nodes,
and a set of unvisited ones.

> data Nearest a = Nearest
>     { visited   :: [a]
>     , unvisited :: S.Set a
>     }

The general Nearest Neighbors algorithm.

> nearestNeighbors' :: (Ord a, Ord b) => (a -> a -> b) -> [a] -> [a]
> nearestNeighbors' d ns = go $ mkNearest ns
>     where
>     -- Our main iteration.
>     -- We're done when there's no more unvisited nodes.
>     go (Nearest vs             us) | S.null us = reverse vs
>     -- Push the nearest onto the front and go with the rest.
>     go (Nearest vs@(current:_) us) =
>         let (next, rest) = nearest current us
>         in go $ Nearest (next:vs) rest
>     -- If we haven't visited anything yet,
>     -- grab the first thing out of the set.
>     go (Nearest []             us) =
>         go $ Nearest [S.findMin us] (S.deleteMin us)
>     -- Compute the nearest distance.
>     nearest i us =
>         let ns = S.map ((,) <*> d i) us
>             n = fst $ minimumBy (comparing snd) ns
>         in (n, S.delete n us)
>
> mkNearest :: Ord a => [a] -> Nearest a
> mkNearest xs =
>     let (vs, us) = splitAt 1 xs in Nearest vs (S.fromList us)
