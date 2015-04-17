{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module P1 where

import Control.Lens (Getting, (^.), (#), (&), (^?), makeLenses)

import Data.Geo.Coordinate ( Longitude, Latitude, _Longitude, _Latitude
                           , _Minutes, _DegreesLatitude, _DegreesLongitude
                           )
import Data.Geo.Geodetic (Sphere, _Sphere, sphericalLaw)
import Data.List (find)
import Data.Maybe (fromJust, mapMaybe)
import Data.Monoid (First)

import Text.Groom (groom)
import Text.Parsec (anyChar, endOfLine, manyTill, spaces, string, try)
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Number (fractional, nat, sign)

data Node = Node
    { _number    :: Int
    , _latitude  :: Latitude
    , _longitude :: Longitude
    } deriving (Eq, Show)

makeLenses ''Node

parseNodes :: Parser [Node]
parseNodes = do
    manyTill anyChar $ try $ string "NODE_COORD_SECTION"
    manyTill parseNode $ try $ string "EOF"

parseNode :: Parser Node
parseNode =
    Node <$> (spaces *> nat)
         <*> parseLatitude
         <*> parseLongitude
         <*  endOfLine

parseLatitude :: Parser Latitude
parseLatitude = parseGeo _Latitude

parseLongitude :: Parser Longitude
parseLongitude = parseGeo _Longitude

parseGeo :: Getting (First a) Double a -> Parser a
parseGeo _Geo = do
    spaces
    dub <- sign <*> fractional :: Parser Double
    dub^?_Geo & fromJust & pure

distance :: Node -> Node -> Int
distance i j = floor $
    sphericalLaw rrr (i^.latitude, i^.longitude) (j^.latitude, j^.longitude)

distance' :: Node -> Node -> Double
distance' i j = rrr' * acos (0.5 * ((1.0 + q1) * q2 - (1.0 - q1) * q3)) + 1.0
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
tour xs = zip xs (t ++ h)
    where
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
    either print (putStrLn . groom . tourDistance' . tour) nodes

optimalNodes :: [Int]
optimalNodes = [1, 14, 13, 12, 7, 6, 15, 5, 11, 9, 10, 19, 20, 21, 16, 3, 2, 17, 22, 4, 18, 8]

optimalTour :: [Node] -> [Node]
optimalTour ns = mapMaybe look optimalNodes
    where
    look o = find ((== o) . _number) ns
