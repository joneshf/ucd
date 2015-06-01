Here we talk about all the algorithms we need.

> module PX2.Algorithm where

> import Control.Lens (_1, _2, _3, view)
>
> import Data.List (sortOn)
> import Data.Tree
>
> import PX2.Graph
>
> import qualified Data.Set as S

> treeShortcut :: (Ord v, Ord w) => Graph v w -> [v]
> treeShortcut = treeShortcut' kruskal

> treeShortcut' :: Ord v => (Graph v w -> MST v w) -> Graph v w -> [v]
> treeShortcut' f graph = walk $ f graph

> walk :: Ord v => MST v w -> [v]
> walk mst = flatten $ head $ unfoldForest go (S.toList $ S.map (view _1) mst)
>     where
>     go x = (x, S.toList $ S.map (view _2) $ S.filter ((== x) . (view _1)) mst)

> type Family v = S.Set (S.Set v)
> kruskal :: (Ord v, Ord w) => Graph v w -> MST v w
> kruskal = go S.empty S.empty . sorted . edges
>     where
>     go :: (Ord v, Ord w) => Family v -> MST v w -> [(v, v, w)] -> MST v w
>     go us a []               = a
>     go us a (e@(i, j, _):es) = case unioned us i j of
>         (Just vs, Just ws)
>             | vs == ws     -> go us                 a              es
>         (Just vs, Just ws) -> go (reunion us vs ws) (S.insert e a) es
>         (Nothing, Just ws) -> go (reunion' us ws i) (S.insert e a) es
>         (Just vs, Nothing) -> go (reunion' us vs j) (S.insert e a) es
>         (Nothing, Nothing) -> go (reunion'' us i j) (S.insert e a) es
>     sorted :: Ord w => S.Set (v, v, w) -> [(v, v, w)]
>     sorted = sortOn (view _3) . S.toList
>     unioned :: Ord v => Family v -> v -> v -> (Maybe (S.Set v), Maybe (S.Set v))
>     unioned us i j = (lookupFamily us i, lookupFamily us j)
>     lookupFamily :: Ord v => Family v -> v -> Maybe (S.Set v)
>     lookupFamily us u = fst <$> S.minView (S.filter (S.member u) us)
>     reunion :: Ord v => Family v -> S.Set v -> S.Set v -> Family v
>     reunion us vs ws = S.insert (S.union vs ws) $ S.delete ws $ S.delete vs us
>     reunion' :: Ord v => Family v -> S.Set v -> v -> Family v
>     reunion' us vs w = reunion us vs (S.singleton w)
>     reunion'' :: Ord v => Family v -> v -> v -> Family v
>     reunion'' us v w = reunion' us (S.singleton v) w

> cycles :: Graph v w -> [S.Set (v, v, w)]
> cycles = undefined

> hasCycles :: Graph v w -> Bool
> hasCycles = not . null . cycles
