We want to verify some facts using tests
that are a bit too time consuming to verify with the type system.

> {-# LANGUAGE OverloadedLists #-}
> module PX2.Test where

> import Control.Lens
>
> import GHC.Natural
>
> import PX2.Algorithm
> import PX2.Graph
>
> import Test.QuickCheck
>
> import qualified Data.Map as M
> import qualified Data.MultiSet as MS
> import qualified Data.Set as S

Generated graphs should have the right number of edges.

> prop_ArbitraryEdgeNumbers :: Graph Natural Natural -> Bool
> prop_ArbitraryEdgeNumbers g = (vSize g * (vSize g - 1) `div` 2) == eSize g

Given a graph G = (V, E), E should be a subset of V x V.

> prop_EdgesAreSubset :: Graph Natural Natural -> Bool
> prop_EdgesAreSubset g = unweightedEdges es `S.isSubsetOf` cartesian vs vs
>     where
>     vs = vertices g
>     es = edges g

A graph with less than three vertices cannot have cycles.

> prop_SmallGraphNoCycles :: Graph Natural Natural -> Property
> prop_SmallGraphNoCycles g = vSize g < 3 ==> not (cycles g)

Given a graph G = (V, E), an MST of G should have exactly max(0, |V| - 1) edges.

> prop_MSTEdges :: MSTFunc Natural Natural
>               -> Graph Natural Natural
>               -> Bool
> prop_MSTEdges f g = max 0 (vSize g - 1) == S.size (f g)

Given a graph G = (V, E), an MST of G should be exactly V.

> prop_MSTVertices :: MSTFunc Natural Natural
>                  -> Graph Natural Natural
>                  -> Property
> prop_MSTVertices f g = vSize g > 1 ==> vertices g == vs
>     where
>     vs = S.fromList $ (^..traverse.each) $ S.toList $ unweightedEdges $ f g

Given a graph G = (V, E), a tree shortcut should be exactly V.

> prop_treeShortcut :: Graph Natural Natural -> Property
> prop_treeShortcut g = vSize g > 1 ==> vertices g == vs
>     where
>     vs = S.fromList $ treeShortcut g

Given a graph G = (V, E), a walk of the MST of G should give exactly V.

> prop_walkVertices :: MSTFunc Natural Natural
>                   -> Graph Natural Natural
>                   -> Property
> prop_walkVertices f g = vSize g > 1 ==> vertices g == vs
>     where
>     vs = S.fromList $ walk $ f g

Given a complete graph G = (V, E), the incidents of E should have a maximum of |V| - 1.

> prop_IncidentsMaximum :: Graph Natural Natural -> Property
> prop_IncidentsMaximum g = vSize g > 1 ==>
>     maximum (incidents $ edges g) == fromIntegral (vSize g - 1)

Given a complete graph G = (V, E), the incidents of E should be exactly V.

> prop_IncidentsKeys :: Graph Natural Natural -> Property
> prop_IncidentsKeys g = vSize g > 1 ==>
>     M.keysSet (incidents $ edges g) == vertices g

Given a graph G = (V, E), a greedy G should be exactly V.

> prop_GreedyVertices :: Graph Natural Natural -> Property
> prop_GreedyVertices g = vSize g > 1 ==> S.fromList (greedy g) == vertices g

Given a graph G = (V, E), a christofides G should be exactly V.

> prop_ChristofidesVertices :: Graph Natural Natural -> Property
> prop_ChristofidesVertices g = vSize g > 1 ==>
>     S.fromList (christofides g) == vertices g

Helper for unit tests/TDD.

> quickAssert :: Testable prop => prop -> IO ()
> quickAssert = quickCheckWith stdArgs { maxSuccess = 1 }

> main :: IO ()
> main = do

>     quickCheck prop_ArbitraryEdgeNumbers
>     quickCheck prop_EdgesAreSubset
>     quickCheckWith stdArgs {maxDiscardRatio = 100} prop_SmallGraphNoCycles
>     quickCheck $ prop_MSTEdges kruskal
>     quickCheck $ prop_MSTVertices kruskal
>     quickCheck $ prop_walkVertices kruskal
>     quickCheck prop_treeShortcut
>     quickCheck prop_IncidentsMaximum
>     quickCheck prop_IncidentsKeys
>     quickCheck prop_GreedyVertices
>     quickCheck prop_ChristofidesVertices

Use some TDD to figure out the walk.

>     quickAssert (walk (S.empty :: MST Natural Natural) == [])
>     quickAssert (walk [(1,2,10)] == [1,2])
>     quickAssert (walk [(1,2,10),(2,3,2)] == [1,2,3])
>     quickAssert (walk [(2,3,0),(2,5,0),(5,6,0)] == [2,3,5,6])
>     quickAssert (walk [(2,10,1),(10,11,2),(8,11,0)] == [2,10,11,8])

Use some TDD to figure out cycles.

>     quickAssert (cycles $ Graph [1,2,3] [(1,2,0),(1,3,0),(2,3,0)])
>     quickAssert (not $ cycles $ Graph [1,2,3] [(1,3,0),(2,3,0)])
>     quickAssert (not $ cycles $ Graph [1..7] [(1,3,0),(2,4,0),(3,6,0),(4,5,0),(5,7,0)])
>     quickAssert (not $ cycles $ Graph [1..7] [(1,2,0),(1,3,0),(2,4,0),(3,6,0),(4,5,0),(5,7,0)])
>     quickAssert (cycles $ Graph [1..7] [(1,2,0),(1,3,0),(2,4,0),(3,6,0),(4,5,0),(5,7,0),(6,7,0)])

Use some TDD to figure out the shortcut.

>     quickAssert (shortcut ([] :: MS.MultiSet (Natural, Natural)) == [])
>     quickAssert (shortcut [(1,2)] == [1,2])
>     quickAssert (shortcut [(1,2),(2,3)] == [1,2,3])
>     quickAssert (shortcut [(2,3),(2,5),(5,6)] == [2,3,5,6])
>     quickAssert (shortcut [(2,10),(10,11),(8,11)] == [2,10,11,8])
>     quickAssert (shortcut [(2,10),(8,11),(10,11)] == [2,10,11,8])
