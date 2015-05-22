Here's the brunt of the program.

We implement the different algorithms here.

> module P4.Morphism where
>
> import Data.Bits (xor)
> import Data.Foldable (find, foldl')
> import Data.Function (on)
> import Data.List (groupBy, maximumBy, minimumBy, sort)
> import Data.Maybe (maybeToList)
> import Data.Ord (comparing)
>
> import P4.Distance
>
> import qualified Data.Set as S

We can compute the `tourLength` of any list of distances.

> tourLength :: [Distance] -> Int
> tourLength = sum . map (_distance . head) . groupBy ((==) `on` _i) . sort

> canonical :: [Distance] -> [Distance]
> canonical = id

> nearestNeighbors :: [Distance] -> [Distance]
> nearestNeighbors [] = []
> nearestNeighbors xs = go (_i $ head xs) xs
>     where
>     go _ [] = []
>     go m xs = let d = shortest m xs
>               in  d:go (next m d) (filter (not . neighbor m) xs)

We codify the "Farthest Insertion" algorithm

> data Mini2Tour a = Mini2Tour a a (S.Set a)
> data Mini3Tour a = Mini3Tour a a a (S.Set a)
> data MiniTour a = MiniTour [a] (S.Set a)

> farthestInsertion :: [Distance] -> [Distance]
> farthestInsertion [] = []
> farthestInsertion xs = runMini $ go $ miniTour $ mini3Tour
>     where
>     runMini :: [Int] -> [Distance]
>     runMini []       = []
>     runMini [_]      = []
>     runMini (x:y:zs) =
>         maybeToList (find ((&&) <$> neighbor x <*> neighbor y) xs) ++ runMini (y:zs)
>     mini2Tour ::Mini2Tour Int
>     mini2Tour = Mini2Tour x y (S.fromList [1..nodes] S.\\ S.fromList [x, y])
>     mini3Tour :: Mini3Tour Int
>     mini3Tour = Mini3Tour x y x (S.delete x us)
>         where
>         Mini2Tour x y us = mini2Tour
>         filtered = (filter (xor <$> neighbor x <*> neighbor y) xs)
>         Distance i' j' _ = maximumBy (comparing _distance) filtered
>         z = if i' == x || i' == y then j' else i'
>     miniTour :: Mini3Tour Int -> MiniTour Int
>     miniTour (Mini3Tour x y z us) = MiniTour [x, y, z] us
>     go :: MiniTour Int -> [Int]
>     go (MiniTour vs us)
>         | S.null us = vs
>         | otherwise = go $ MiniTour vs' (S.difference us $ S.fromList vs')
>         where
>         filtered = filter (\d -> 1 == (length $ filter (`neighbor` d) vs)) xs
>         Distance i' j' _ = maximumBy (comparing _distance) filtered
>         vs' = foldl' (inject i' j') [] vs
>     inject i' j' acc v
>             | v == i'   = acc ++ [v] ++ [j']
>             | v == j'   = acc ++ [v] ++ [i']
>             | otherwise = acc ++         [v]
>     farthest = maximumBy (comparing _distance) xs
>     (x, y) = (_i farthest, _j farthest)
>     nodes = floor $ (1 + sqrt (fromIntegral (1 + 8 * length xs))) / 2

-- > data Farthest a = Farthest
-- >     { visited   :: G.Graph
-- >     , unvisited :: S.Set Int
-- >     }

-- > farthestInsertion :: [Distance] -> [Distance]
-- > farthestInsertion [] = []
-- > farthestInsertion xs = fromMaybe [] $ go mkFarthest xs
-- >     where
-- >     go (Nearest vs us)
-- >         | S.null us = runFarthest (G.edges vs)
-- >     next vs us = maximumBy (comparing _distance) $ do
-- >         u <- us
-- >         e <- G.nodes vs
-- >         pure $ longest u (filter (neighbor e) xs)
-- >     runFarthest = traverse (\e -> _distance <$> find (\d -> src e == _i d && tgt e == _j d) xs)
-- >     nodes = floor $ (1 + sqrt (fromIntegral (1 + 8 * length xs))) / 2
-- >     len = length xs
-- >     farthest = maximumBy (comparing _distance) xs
-- >     mkFarthest = Farthest (G.fromEdges [Edge x y, Edge y x]) (S.fromList [1..nodes] \\ S.fromList [x, y])
-- >     (x, y) = (_i farthest, _j farthest)

> shortest :: Int -> [Distance] -> Distance
> shortest n = minimumBy (comparing _distance) . filter (neighbor n)
> longest :: Int -> [Distance] -> Distance
> longest n = maximumBy (comparing _distance) . filter (neighbor n)
> next :: Int -> Distance -> Int
> next n (Distance i j _) = if i == n then j else i
> neighbor :: Int -> Distance -> Bool
> neighbor n (Distance i j _) = i == n || j == n

> twoOpt :: [Distance] -> [Distance]
> twoOpt = undefined

> lin'Kernighan :: [Distance] -> [Distance]
> lin'Kernighan = undefined

