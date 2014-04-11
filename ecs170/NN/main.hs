module Main where

import Data.Array
import Data.List
import Data.Monoid

import System.Environment
import System.Random

import qualified Data.Foldable as F

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

data BackNetwork = BackNetwork { bLayers :: [Layer]
                               , bRate   :: Double
                               }

data PropLayer = PropLayer { plIn :: ColumnVector Double
                           , plOut :: ColumnVector Double
                           , pFA :: ColumnVector Double
                           , pWeight :: Matrix Double
                           , pASpec :: ASpec
                           }
               | PropInputLayer { pilOut :: ColumnVector Double }

data BackPropLayer = BackPropLayer { bpDel     :: ColumnVector Double
                                   , bpErrGrad :: ColumnVector Double
                                   , bpFA      :: ColumnVector Double
                                   , bpIn      :: ColumnVector Double
                                   , bpOut     :: ColumnVector Double
                                   , bpWeight  :: Matrix Double
                                   , bpASpec   :: ASpec
                                   }

instance Num t => Monoid (Matrix t) where
    mempty = Matrix 0 0 (array (0, 0) [])
    x `mappend` y = Matrix rx cy dat
        where
            dat = fromList rx cy arr
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

trans :: Matrix a -> Matrix a
trans Matrix{mrows = r, mcols = c, mdat = dat} = Matrix c r dat'
    where
        foo = assocs dat
        foo' = [(r' * r + c', e)
               | r' <- [0..(c - 1)]
               , c' <- [1..r]
               , (_, e) <- foo
               ]
        dat' = array (bounds dat) foo'

chunks :: Int -> [a] -> [[a]]
chunks n = takeWhile (not . null) . unfoldr (Just . splitAt n)

fromList :: (Ix i, Num i, Enum i) => i -> i -> [e] -> Array i e
fromList row col vals = array (0, row * col) (zip [1..] vals)

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

backpropagate :: PropLayer -> BackPropLayer -> BackPropLayer
backpropagate lj lk = BackPropLayer delJ eGrad fAJ bpIn' bpOut' bpWeight' bpASpec'
    where
        delJ = wKT <> delK <> fAK
        delK = bpDel lk
        wKT = trans (bpWeight lk)
        fAK = bpFA lk
        fAJ = pFA lj
        eGrad = errorGrad delJ fAJ (plIn lj)
        bpIn' = plIn lj
        bpOut' = plOut lj
        bpWeight' = pWeight lj
        bpASpec' = pASpec lj

backpropagateFinal :: PropLayer -> ColumnVector Double -> BackPropLayer
backpropagateFinal l t = BackPropLayer del eGrad pFA' pIn' pOut' pWeight' pASpec'
    where
        del = plOut l `sub` t
        fA = pFA l
        eGrad = errorGrad del fA (plIn l)
        pFA' = pFA l
        pIn' = plIn l
        pOut' = plOut l
        pWeight' = pWeight l
        pASpec' = pASpec l

sub :: ColumnVector Double -> ColumnVector Double -> ColumnVector Double
sub Matrix{mrows = r, mcols = c, mdat = d1} Matrix{mdat = d2} =
    Matrix r c $ array (bounds d1) (zipWith (\(i, e1) (_, e2) -> (i, e1 - e2)) (assocs d1) (assocs d2))

backpropagateNetwork :: ColumnVector Double -> [PropLayer] -> [BackPropLayer]
backpropagateNetwork target layers = scanr backpropagate ll hidden
    where
        hidden = init layers
        ll = backpropagateFinal (last layers) target

buildBackpropNet ::
  -- The learning rate
  Double ->
  -- The weights for each layer
  [Matrix Double] ->
  -- The activation specification (used for all layers)
  ASpec ->
  -- The network
  BackNetwork
buildBackpropNet lr ws s = BackNetwork ls lr
  where checkedWeights = scanl1 checkDims ws
        ls = map buildLayer checkedWeights
        buildLayer w = Layer { lWeight=w, lASpec=s }

update :: Double -> BackPropLayer -> Layer
update rate layer = Layer newWeight (bpASpec layer)
    where
        oldWeight = bpWeight layer
        delWeight = fmap (* rate) (bpErrGrad layer)
        newWeight = oldWeight `sub` delWeight

errorGrad :: ColumnVector Double -> ColumnVector Double -> ColumnVector Double -> Matrix Double
errorGrad del fA input = del <> fA <> trans input

validate :: Network -> ColumnVector Double -> Maybe (ColumnVector Double)
validate Network{nLayers = []} _ = Nothing
validate Network{nLayers = (n:ns)} input = if good then Just input else Nothing
    where
        good = r == r' && inBound
        r = mrows input
        r' = mrows $ lWeight n
        weights = concatMap (toList . mdat . lWeight) (n:ns)
        inBound = F.all (\x -> 0 <= x && x <= 1) weights

makTrix :: Int -> Int -> [t] -> Matrix t
makTrix rows cols = Matrix rows cols . fromList rows cols

smallRandoms :: Int -> [Double]
smallRandoms seed = map (/100) (randoms (mkStdGen seed))

randomWeight :: Int -> Int -> Int -> Matrix Double
randomWeight numInputs numOutputs seed = makTrix numOutputs numInputs weights
    where weights = take (numOutputs*numInputs) (smallRandoms seed)

zeroWeight :: Int -> Int -> Matrix Double
zeroWeight numInputs numOutputs = makTrix numOutputs numInputs weights
    where weights = repeat 0

learningRate = 0.8

readTrainingData :: IO []

main :: IO ()
main = do
    let w1 = randomWeight (28*28 + 1) 20 7
    let w2 = randomWeight 20 10 42
    let initialNet = buildBackpropNet learningRate [w1, w2] tanhASpec
    trainingData2 <- readTrainingData
    let trainingData = take 20000 trainingData2
    putStrLn $ "Training with " ++ show (length trainingData) ++ " images"
    let finalNet = trainWithAllPatterns initialNet trainingData
    testData2 <- readTestData
    let testData = take 1000 testData2
    putStrLn $ "Testing with " ++ show (length testData) ++ " images"
    let results = evalAllPatterns finalNet testData
    let score = fromIntegral (sum results)
    let count = fromIntegral (length testData)
    let percentage = 100.0 * score / count
    putStrLn $ "I got " ++ show percentage ++ "% correct"


xorTarget :: [[Double]]
xorTarget = [
            ]
