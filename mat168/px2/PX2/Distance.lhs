We want to represent the distance as
a pair of node numbers and the distance between them.

> module PX2.Distance where
>
> import qualified Data.Set as S
>
> data Distance = Distance
>     { i        :: Int
>     , j        :: Int
>     , distance :: Int
>     } deriving (Eq, Ord, Show)

> distanceEdge :: Distance -> (Int, Int, Int)
> distanceEdge = (,,) <$> i <*> j <*> distance

> distancesEdges :: [Distance] -> S.Set (Int, Int, Int)
> distancesEdges = S.fromList . fmap distanceEdge

> distancesVertices :: [Distance] -> S.Set Int
> distancesVertices ds = isjs
>     where
>     is = S.fromList $ i <$> ds
>     js = S.fromList $ j <$> ds
>     isjs = is `S.union` js
