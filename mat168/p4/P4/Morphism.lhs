Here's the brunt of the program.

We implement the different algorithms here.

> module P4.Morphism where
>
> import Data.Bits (xor)
> import Data.Foldable (find, foldl')
> import Data.Function ((&), on)
> import Data.List (groupBy, maximumBy, minimumBy, sort)
> import Data.Maybe (fromMaybe, isJust, maybeToList)
> import Data.Ord (comparing)
> import Data.Traversable (for)
> import Data.Tuple (swap)
>
> import P4.Distance
>
> import qualified Data.Set as S

We can compute the `tourLength` of any list of distances.

> tourLength :: [Distance] -> Int
> tourLength = sum . map _distance

> canonical :: [Distance] -> [Distance]
> canonical xs = front ++ loop
>     where
>     front = map head . groupBy ((==) `on` _i) . sort $ xs
>     loop = take 1 . map last . groupBy ((==) `on` _i) . sort $ xs

> nearestNeighbors :: [Distance] -> [Distance]
> nearestNeighbors [] = []
> nearestNeighbors xs = go (_i $ head xs) xs
>     where
>     go _ [] = []
>     go m xs = let d = shortest m xs
>               in  d:go (next m d) (filter (not . neighbor m) xs)

We codify the "Farthest Insertion" algorithm

> data Mini2Tour a = Mini2Tour a a   (S.Set a)
> data Mini3Tour a = Mini3Tour a a a (S.Set a)
> data MiniTour  a = MiniTour [a]    (S.Set a)

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
>     mini2Tour = Mini2Tour x y (S.fromList [1..nodes xs] S.\\ S.fromList [x, y])
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

> nodes :: [a] -> Int
> nodes xs = floor $ (1 + sqrt (fromIntegral (1 + 8 * length xs))) / 2
> shortest :: Int -> [Distance] -> Distance
> shortest n = minimumBy (comparing _distance) . filter (neighbor n)
> longest :: Int -> [Distance] -> Distance
> longest n = maximumBy (comparing _distance) . filter (neighbor n)
> next :: Int -> Distance -> Int
> next n (Distance i j _) = if i == n then j else i
> neighbor :: Int -> Distance -> Bool
> neighbor n (Distance i j _) = i == n || j == n

> twoOpt :: [Distance] -> [Distance]
> twoOpt xs = go (canonical xs)
>     where
>     go :: [Distance] -> [Distance]
>     go ys = case improvement xs nodeNums ys of
>         []  -> ys
>         ys' -> go (minimumBy (comparing tourLength) ys')
>     nodeNums = [1..nodes xs]

> improvement :: [Distance] -> [Int] -> [Distance] -> [[Distance]]
> improvement xs ys dist = do
>     i <- [0..length ys - 1]
>     k <- [i + 1..length ys]
>     let ys' = take 1 ys ++ twoOptSwap (drop 1 ys) i k
>     case twoOptDistance xs ys' of
>         Just dist' -> if tourLength dist' < tourLength dist then
>                           [dist']
>                       else
>                           []
>         Nothing    -> []
> twoOptDistance :: [Distance] -> [Int] -> Maybe [Distance]
> twoOptDistance xs ys = for (pair ys) $ \(i', j') ->
>     find (\d -> _i d == min i' j' && _j d == max i' j') xs
> pair = zip <*> (uncurry (++) . swap . splitAt 1)
> twoOptSwap :: [a] -> Int -> Int -> [a]
> twoOptSwap xs i k = front ++ reverse middle ++ back
>     where
>     (front, (middle, back)) = splitAt (k - i) <$> splitAt (i - 1) xs

> lin'Kernighan :: [Distance] -> [Distance]
> lin'Kernighan = undefined

> α = 3
> improvePath :: [Distance] -> [Int] -> Int -> S.Set Int -> [Int]
> improvePath xs path depth restricted
>     | depth < α = do
>         (x, y) <- filter (flip S.notMember restricted . fst) $ pair path
>         if (weight xs) x y > (weight xs) (last path) x then (go xs path depth restricted) x y else []
>     | otherwise = uncurry (go xs path depth restricted) . maximumBy (comparing (\(x, y) -> (weight xs) x y - (weight xs) (last path) x)) . init . pair $ path
>         where
>         e = last path
> go xs path depth restricted x y =
>     let swapped = replace path y (last path)
>         swappedDistance = tourLength <$> twoOptDistance xs swapped
>         pathDistance = tourLength <$> twoOptDistance xs path
>         bothJusts = ((&&) `on` isJust) swappedDistance pathDistance
>     in  if bothJusts && swappedDistance < pathDistance then
>             swapped ++ take 1 swapped
>         else
>             improvePath xs swapped (depth + 1) (S.insert x restricted)
> replace path y e =
>     let (front,  back) = span ((/=) y) path
>         (middle, rest) = span ((/=) e) $ drop 1 back
>     in  front ++ [e] ++ middle ++ [y] ++ drop 1 rest
> weight :: [Distance] -> Int -> Int -> Int
> weight xs x y = maybe 0 _distance $ find ((&&) <$> neighbor x <*> neighbor y) xs


Algorithm 1 ImprovePath(P, depth, R) recursive algorithm (LKtsp version). The function
either terminates after an improved tour is found or finishes normally with no profit.
Require: The path P = b → . . . → e, recursion depth depth and a set of restricted
vertices R.
if depth < α then
  for every edge x → y ∈ P such that x /∈ R do
    Calculate g = w(x → y) − w(e → x) (see Figure 1b).
    if g > 0 then
      if the tour b → . . . → x → e → . . . → y → b is an improvement over the original one then
        Accept the produced tour and terminate.
      else
        ImprovePath(b → . . . → x → e → . . . → y, depth + 1, R ∪ {x}).
else
  Find the edge x → y which maximizes g = w(x → y) − w(e → x).
    if g > 0 then
      if the tour b → . . . → x → e → . . . → y → b is an improvement over the original one then
        Accept the produced tour and terminate.
      else
        return ImprovePath(b → . . . → x → e → . . . → y, depth + 1, R ∪ {x}).
