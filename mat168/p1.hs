{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module P1 where

import Control.Lens

import Data.Char (digitToInt,isUpper)
import Data.Geo.Coordinate
import Data.Geo.Geodetic
import Data.List
import Data.Maybe

import Text.Groom
import Text.Parsec (anyChar, chainl1, char, choice, digit, endOfLine, manyTill, satisfy, spaces, string, try)
import Text.Parsec.String (Parser, parseFromFile)
import Text.Printf

data Node = Node
    { _number    :: Int
    , _latitude  :: Latitude
    , _longitude :: Longitude
    } deriving (Eq, Show)

makeLenses ''Node

parseNodes :: Parser [Node]
parseNodes = do
    manyTill anyChar $ try $ string "NODE_COORD_SECTION"
    manyTill parseNode $ try $ satisfy isUpper

parseNode :: Parser Node
parseNode =
    Node <$> (spaces *> parseInt)
         <*> parseLatitude
         <*> parseLongitude
         <*  endOfLine

parseLatitude :: Parser Latitude
parseLatitude = spaces *> choice [pos, neg, lat]
    where
    pos = char '+' *> lat
    neg = char '-' *> neglat
    s = remSeconds 0
    lat = do
        d <- parseInt
        char '.'
        m <- parseInt
        let dub = read $ printf "%d.%d" d m :: Double
        dub^?_Latitude & fromJust & pure
    neglat = do
        d <- negate <$> parseInt
        char '.'
        m <- parseInt
        let dub = read $ printf "%d.%d" d m :: Double
        dub^?_Latitude & fromJust & pure

parseLongitude :: Parser Longitude
parseLongitude = spaces *> choice [pos, neg, long]
    where
    pos = char '+' *> long
    neg = char '-' *> neglong
    s = remSeconds 0
    long = do
        d <- parseInt
        char '.'
        m <- parseInt
        let dub = read $ printf "%d.%d" d m :: Double
        dub^?_Longitude & fromJust & pure
    neglong = do
        d <- negate <$> parseInt
        char '.'
        m <- parseInt
        let dub = read $ printf "%d.%d" d m :: Double
        dub^?_Longitude & fromJust & pure

parseInt :: Parser Int
parseInt = chainl1 (digitToInt <$> digit) (pure $ (+) . (* 10))

distance :: Node -> Node -> Int
distance i j = floor $
    sphericalLaw rrr (i^.latitude, i^.longitude) (j^.latitude, j^.longitude)

distance' :: Node -> Node -> Double
distance' i j = rrr' * acos (0.5 * (1.0 + q1) * q2 - (1.0 - q1) * q3) + 1.0
    where
    rrr' = _Sphere # rrr
    q1 = cos (longI - longJ)
    q2 = cos (latI - latJ)
    q3 = cos (latI + latJ)
    latI = pi * (fromIntegral dLatI + 5.0 * fromIntegral mLatI / 3.0) / 180.0
    latJ = pi * (fromIntegral dLatJ + 5.0 * fromIntegral mLatJ / 3.0) / 180.0
    longI = pi * (fromIntegral dLongI + 5.0 * fromIntegral mLongI / 3.0) / 180.0
    longJ = pi * (fromIntegral dLongJ + 5.0 * fromIntegral mLongJ / 3.0) / 180.0
    dLatI :: Int
    dLatI = _DegreesLatitude # (i^.latitude._DegreesLatitude)
    dLatJ :: Int
    dLatJ = _DegreesLatitude # (j^.latitude._DegreesLatitude)
    dLongI :: Int
    dLongI = _DegreesLongitude # (i^.longitude._DegreesLongitude)
    dLongJ :: Int
    dLongJ = _DegreesLongitude # (j^.longitude._DegreesLongitude)
    mLatI :: Int
    mLatI = _Minutes # (i^.latitude._Minutes)
    mLatJ :: Int
    mLatJ = _Minutes # (j^.latitude._Minutes)
    mLongI :: Int
    mLongI = _Minutes # (i^.longitude._Minutes)
    mLongJ :: Int
    mLongJ = _Minutes # (j^.longitude._Minutes)

rrr :: Sphere
rrr = (6378.388 :: Double)^._Sphere

tour :: [Node] -> [(Node, Node)]
tour xs = zip xs rot
    where
    rot = t ++ h
    (h, t) = splitAt 1 xs

tourDistance :: [(Node, Node)] -> Int
tourDistance = sum . fmap (uncurry distance)

tourDistance' :: [(Node, Node)] -> Double
tourDistance' = sum . fmap (uncurry distance')

main :: IO ()
main = do
    nodes <- parseFromFile parseNodes "ulysses22.tsp"
    either print (putStrLn . groom . tourDistance . tour . optimalTour) nodes
    either print (putStrLn . groom . tourDistance' . tour . optimalTour) nodes
    either print (putStrLn . groom . tourDistance . tour) nodes
    --either print (putStrLn . groom . tour . optimalTour) nodes

optimalNodes :: [Int]
optimalNodes = [1, 14, 13, 12, 7, 6, 15, 5, 11, 9, 10, 19, 20, 21, 16, 3, 2, 17, 22, 4, 18, 8]

optimalTour :: [Node] -> [Node]
optimalTour ns = mapMaybe look optimalNodes
    where
    look o = find ((== o) . _number) ns
