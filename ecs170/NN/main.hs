module Main where

import Data.Array
import qualified Data.Foldable as F
import Data.List
import Data.Monoid

data Matrix t = Matrix { mrows :: Int
                       , mcols :: Int
                       , mdat  :: Array Int t
                       }

type ColumnVector t = Matrix t

data ASpec = ASpec { asF    :: Double -> Double
                   , asF'   :: Double -> Double
                   , asDesc :: String
                   }

data Layer = Layer { lWeight :: Matrix Double
                   , lASpec  :: ASpec
                   }

data Network = Network { nLayers :: [Layer]
                       , nRate   :: Double
                       }

data PropLayer = PropLayer { plIn :: ColumnVector Double
                           , plOut :: ColumnVector Double
                           , pFA :: ColumnVector Double
                           , pWeight :: Matrix Double
                           , pASpec :: ASpec
                           }
               | PropInputLayer { pilOut :: ColumnVector Double }

instance Num t => Monoid (Matrix t) where
    mempty = Matrix 0 0 (array (0, 0) [])
    x `mappend` y = Matrix rx cy dat
        where
            dat = array (0, rx * cy) (zip [1..] arr)
            rx = mrows x
            ry = mrows y
            cy = mcols y
            xList = transpose . chunks rx . toList $ mdat x
            yList = chunks ry . toList $ mdat y
            arr = [ sum $ zipWith (*) xcol ycol
                  | ycol <- yList
                  , xcol <- xList
                  ]

instance Functor Matrix where
    fmap f m@Matrix{mdat = dat} = m {mdat = fmap f dat}

instance F.Foldable Matrix where
    foldr f z Matrix{mdat = dat} = F.foldr f z dat

--transpose :: Matrix a -> Matrix a
--transpose Matrix{mrows = r, mcols = c, mdat = dat} = Matrix c r dat'
--    where
--        foo = assocs dat
--        foo' = [(r' * r + c', e)
--               | r' <- [0..(c - 1)]
--               , c' <- [1..r]
--               , (_, e) <- foo
--               ]
--        dat' = array (bounds dat) foo'

chunks :: Int -> [a] -> [[a]]
chunks n = takeWhile (not . null) . unfoldr (Just . splitAt n)

toList :: Ix i => Array i e -> [e]
toList a = map snd (assocs a)

tanhASpec :: ASpec
tanhASpec = ASpec tanh tanh' "tanh"

tanh' :: Double -> Double
tanh' x = 1 - tanh x ** 2

checkDims :: Maybe (Matrix Double) -> Maybe (Matrix Double) -> Maybe (Matrix Double)
checkDims m1 m2 = do
    Matrix{mrows = r} <- m1
    Matrix{mcols = c} <- m2
    if r == c then m2 else Nothing

buildNetwork :: Double -> [Matrix Double] -> ASpec -> Maybe Network
buildNetwork rate weights aspec = do
    let maybeWeights = map Just weights
    checked <- sequence $ scanl1 checkDims maybeWeights
    let ls = map buildLayer checked
    return $ Network ls rate
    where
        buildLayer weight = Layer weight aspec

propagate :: PropLayer -> Layer -> PropLayer
propagate PropLayer{plOut = x} lk = PropLayer x y fA w (lASpec lk)
    where
        w = lWeight lk
        a = w <> x
        f = asF $ lASpec lk
        f' = asF' $ lASpec lk
        y = fmap f a
        fA = fmap f' a

propagateNetwork :: ColumnVector Double -> Network -> Maybe [PropLayer]
propagateNetwork input net = do
    valid <-  validate net input
    let layer' = PropInputLayer valid
    let calcs = scanl propagate layer' (nLayers net)
    return $ tail calcs

validate :: Network -> ColumnVector Double -> Maybe (ColumnVector Double)
validate Network{nLayers = []} _ = Nothing
validate Network{nLayers = (n:ns)} input = if good then Just input else Nothing
    where
        good = r == r' && inBound
        r = mrows input
        r' = mrows $ lWeight n
        weights = concatMap (toList . mdat . lWeight) (n:ns)
        inBound = F.all (\x -> 0 <= x && x <= 1) weights

main :: IO ()
main = putStrLn "Hello World"
