module Main where

import AI.HNN.FF.Network (Sample, (-->), createNetwork, output, quadError, sigmoid, sigmoid', tanh', trainNTimes, trainUntil)

import Control.Applicative ((<$>), (<*>), (<*), (<|>))

import Data.Foldable (for_, maximumBy)
import Data.Function (on)

import Debug.Trace (traceShow)

import Numeric.LinearAlgebra (Vector, fromList, toList)

import Text.Parsec (eof, many, many1, newline, noneOf, spaces, string)
import Text.Parsec.String (Parser, parseFromFile)

data Yeast = Yeast
    { name         :: String
    , mcg          :: Double
    , gvh          :: Double
    , alm          :: Double
    , mit          :: Double
    , erl          :: Double
    , pox          :: Double
    , vac          :: Double
    , nuc          :: Double
    , localization :: LocalizationSite
    } deriving Show

data LocalizationSite = CYT
                      | NUC
                      | MIT
                      | ME3
                      | ME2
                      | ME1
                      | EXC
                      | VAC
                      | POX
                      | ERL
                      deriving (Bounded, Enum, Read, Show)

yeastInput :: Yeast -> Vector Double
yeastInput y = fromList $ ($ y) <$> [mcg, gvh, alm, mit, erl, pox, vac, nuc]

yeastOutput :: Yeast -> Vector Double
yeastOutput Yeast{localization = CYT} = fromList [1, 0, 0, 0, 0, 0, 0, 0, 0, 0]
yeastOutput Yeast{localization = NUC} = fromList [0, 1, 0, 0, 0, 0, 0, 0, 0, 0]
yeastOutput Yeast{localization = MIT} = fromList [0, 0, 1, 0, 0, 0, 0, 0, 0, 0]
yeastOutput Yeast{localization = ME3} = fromList [0, 0, 0, 1, 0, 0, 0, 0, 0, 0]
yeastOutput Yeast{localization = ME2} = fromList [0, 0, 0, 0, 1, 0, 0, 0, 0, 0]
yeastOutput Yeast{localization = ME1} = fromList [0, 0, 0, 0, 0, 1, 0, 0, 0, 0]
yeastOutput Yeast{localization = EXC} = fromList [0, 0, 0, 0, 0, 0, 1, 0, 0, 0]
yeastOutput Yeast{localization = VAC} = fromList [0, 0, 0, 0, 0, 0, 0, 1, 0, 0]
yeastOutput Yeast{localization = POX} = fromList [0, 0, 0, 0, 0, 0, 0, 0, 1, 0]
yeastOutput Yeast{localization = ERL} = fromList [0, 0, 0, 0, 0, 0, 0, 0, 0, 1]

parseYeasts :: Parser [Yeast]
parseYeasts = many parseYeast

parseYeast :: Parser Yeast
parseYeast = Yeast <$> parseString
                   <*> parseDouble
                   <*> parseDouble
                   <*> parseDouble
                   <*> parseDouble
                   <*> parseDouble
                   <*> parseDouble
                   <*> parseDouble
                   <*> parseDouble
                   <*> parseLocalizationSite

parseString :: Parser String
parseString = many1 (noneOf " \n") <* spaces

parseDouble :: Parser Double
parseDouble = read <$> parseString

parseLocalizationSite :: Parser LocalizationSite
parseLocalizationSite = read <$> parseString

take75 :: [a] -> [a]
take75 xs = take (floor $ 0.75 * (fromInteger $ toInteger $ length xs)) xs

drop75 :: [a] -> [a]
drop75 xs = drop (floor $ 0.75 * (fromInteger $ toInteger $ length xs)) xs

training :: [Yeast] -> [Sample Double]
training = (yeastSample <$>) . take75

testing :: [Yeast] -> [Sample Double]
testing = (yeastSample <$>) . drop75

yeastSample :: Yeast -> Sample Double
yeastSample = (-->) <$> yeastInput <*> yeastOutput

classification :: Vector Double -> LocalizationSite
classification =
    fst . maximumBy (compare `on` snd) . zip (enumFrom minBound) . toList

main :: IO ()
main = do
    yeasts <- parseFromFile parseYeasts "yeast.data"
    net <- createNetwork 8 [3] 10
    let training' = either (const []) training yeasts
    let testing' = either (const []) testing yeasts
    --for_ training' $ print . output net sigmoid . fst

    print "------------------------------------------"
    print "Trained"
    print "------------------------------------------"

    --let trained = trainNTimes 1000 0.8 sigmoid sigmoid' net training'
    let trained = trainUntil (\_ n s -> traceShow (quadError tanh n s) False) 0.9 tanh tanh' net training'

    --for_ training' $ print . output trained sigmoid . fst

    print "------------------------------------------"
    print "Classification"
    print "------------------------------------------"

    for_ testing' $ print . classification . output trained tanh . fst
