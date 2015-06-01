Here we talk about all the algorithms we need.

> {-# LANGUAGE OverloadedLists #-}
> {-# LANGUAGE ViewPatterns #-}
> module PX2.Algorithm where

> import Control.Lens ((|>), (^..), _1, _2, _3, each, view)
>
> import Data.Function (on)
> import Data.List (group, sort, sortOn)
> import Data.List.Split (chunksOf)
> import Data.Tree
>
> import GHC.Exts
> import GHC.Natural
>
> import PX2.Graph
> import PX2.Orphan
>
> import qualified Data.Map as M
> import qualified Data.MultiSet as MS
> import qualified Data.Set as S

> treeShortcut :: (Ord v, Ord w) => Graph v w -> [v]
> treeShortcut = treeShortcut' kruskal

> treeShortcut' :: Ord v => MSTFunc v w -> Graph v w -> [v]
> treeShortcut' f graph = walk $ f graph

We don't convert this to an actual tree, so the walk is a bit odd.
It's possible that it's not even correct,
but it seems to do the job.

> walk :: Ord v => MST v w -> [v]
> walk mst = go S.empty (S.toAscList . unweightedEdges $ mst)
>     where
>     go :: Ord v => S.Set v -> [(v, v)] -> [v]
>     go vs []              = []
>     go vs es@((i, j):_) = snd (go' vs i es)
>         where
>         -- This is pretty terrible...
>         go' vs i []
>             | S.member i vs = (vs, [])
>             | otherwise     = (S.insert i vs, [i])
>         go' vs i ((j, k):es')
>             | not (S.member i vs)           =
>                 (i:) <$> go' (S.insert i vs) i ((j, k):es')
>             | i == k && not (S.member j vs) =
>                 let (vs', xs) = (j:) <$> go' (S.insert j vs) j es
>                 in  (xs ++) <$> go' vs' i es'
>             | i == j && not (S.member k vs) =
>                 let (vs', xs) = (k:) <$> go' (S.insert k vs) k es
>                 in  (xs ++) <$> go' vs' i es'
>             | otherwise                     =
>                 go' vs i es'

> kruskal :: (Ord v, Ord w) => MSTFunc v w
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

> unioned :: Ord v => Family v -> v -> v -> (Maybe (S.Set v), Maybe (S.Set v))
> unioned us i j = (lookupFamily us i, lookupFamily us j)
> lookupFamily :: Ord v => Family v -> v -> Maybe (S.Set v)
> lookupFamily us u = fst <$> S.minView (S.filter (S.member u) us)
> reunion :: Ord v => Family v -> S.Set v -> S.Set v -> Family v
> reunion us vs ws = S.insert (S.union vs ws) $ S.delete ws $ S.delete vs us
> reunion' :: Ord v => Family v -> S.Set v -> v -> Family v
> reunion' us vs w = reunion us vs (S.singleton w)
> reunion'' :: Ord v => Family v -> v -> v -> Family v
> reunion'' us v w = reunion' us (S.singleton v) w

> cycles :: Ord v => Graph v w -> Bool
> cycles g = vSize g > 2 && go S.empty (S.toList $ unweightedEdges $ edges g)
>     where
>     go :: Ord v => Family v -> [(v, v)] -> Bool
>     go us [] = False
>     go us ((i, j):es) = case unioned us i j of
>         (Just vs, Just ws) | vs == ws     -> True
>         (Just vs, Just ws)                -> go (reunion us vs ws) es
>         (Just vs, Nothing)                -> go (reunion' us vs j) es
>         (Nothing, Just ws)                -> go (reunion' us ws i) es
>         (Nothing, Nothing)                -> go (reunion'' us i j) es

> incidents :: Ord v => S.Set (v, v, w) -> M.Map v Natural
> incidents = foldMap (M.singleton <$> head <*> fromIntegral . length)
>           . group
>           . sort
>           . (^..traverse.each)
>           . S.toList
>           . unweightedEdges

> tooIncident :: Ord v => S.Set (v, v, w) -> Bool
> tooIncident = tooIncident' 2

> tooIncident' :: Ord v => Natural -> S.Set (v, v, w) -> Bool
> tooIncident' n es = n < maximum (incidents es)

> greedy :: (Ord v, Ord w) => Graph v w -> [v]
> greedy g = go S.empty . sortOn (view _3) . S.toList . edges $ g
>     where
>     go ps _ | vSize g - 1 == S.size ps = walk ps
>     go ps []                           = walk ps
>     go ps (e:es)                       = let ps' = S.insert e ps in
>         if cycles g {edges = ps'} || tooIncident ps'
>           then go ps es
>           else go ps' es

> christofides :: (Ord v, Ord w) => Graph v w -> [v]
> christofides = christofides' kruskal

> christofides' :: (Ord v, Ord w) => MSTFunc v w -> Graph v w -> [v]
> christofides' f g = shortcut $ multigraph mst perfects
>     where
>     mst = f g
>     odds = oddIncidents mst
>     perfects = perfectMatching g odds
>     multigraph = MS.union `on` MS.fromSet . unweightedEdges

We can eschew creating an Eulerian Tour,
and just shortcut the multigraph.

> type Missed v = [(v, v)]
> type Edges v = [(v, v)]
> shortcut :: Ord v => MS.MultiSet (v, v) -> [v]
> shortcut []                    = []
> shortcut (toList -> (i, j):es) = go [i, j] [i, j] es
>     where
>     -- This is pretty terrible...
>     go :: Ord v => S.Set v -> [v] -> [(v, v)] -> [v]
>     go ws vs [] = vs
>     go ws vs ((i,j):es)
>         | S.member i ws && S.member j ws
>             = go                 ws   vs            es
>         | S.member i ws
>             = go (S.union [j]    ws) (vs |> j)      es
>         | S.member j ws
>             = go (S.union [i]    ws) (vs |> i)      es
>         | otherwise
>             = go (S.union [i, j] ws) (vs |> j |> i) es

> perfectMatching :: (Ord v, Ord w) => Graph v w -> [v] -> S.Set (v, v, w)
> perfectMatching g vs = foldl go S.empty
>                      . filter ((== 2) . length)
>                      . chunksOf 2
>                      . sort
>                      $ vs
>     where
>     es = edges g
>     go acc [x, y] = S.union acc $ S.filter (equivalent x y) es
>     equivalent x y (i, j, _) = i == x && j == y

> oddIncidents :: Ord v => S.Set (v, v, w) -> [v]
> oddIncidents = M.keys . M.filter odd . incidents
