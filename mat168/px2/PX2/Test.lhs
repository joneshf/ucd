We want to verify some facts using tests
that are a bit more complex to verify with the type system.

> module PX2.Test where

> import GHC.Natural
>
> import PX2.Algorithm
> import PX2.Graph
>
> import Test.QuickCheck
>
> import qualified Data.Set as S

Given a graph G = (V, E), E should be a subset of V x V.

> prop_EdgesAreSubset :: Graph Natural Natural -> Bool
> prop_EdgesAreSubset g = unweightedEdges g `S.isSubsetOf` cartesian vs vs
>     where vs = vertices g

A graph with less than three vertices cannot have cycles.

> prop_SmallGraphNoCycles :: Graph Natural Natural -> Property
> prop_SmallGraphNoCycles g = S.size (vertices g) < 3 ==> not (hasCycles g)

Given a graph G = (V, E), an MST of G should have exactly max(0, |V| - 1) edges.

> prop_MSTEdges :: (Graph Natural Natural -> MST Natural Natural)
>               -> Graph Natural Natural
>               -> Bool
> prop_MSTEdges f g = max 0 (S.size (vertices g) - 1) == S.size (f g)

> main :: IO ()
> main = do
>     quickCheck prop_EdgesAreSubset
>     --quickCheckWith stdArgs {maxDiscardRatio = 100} prop_SmallGraphNoCycles
>     quickCheck $ prop_MSTEdges kruskal
