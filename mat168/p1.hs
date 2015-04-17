{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module P1 where

import Control.Lens

import Data.Geo.Geodetic (Sphere, _Sphere)
import Data.List (find)
import Data.Maybe (mapMaybe)

import Debug.Trace

import Text.Groom (groom)
import Text.Parsec (anyChar, endOfLine, manyTill, spaces, string, try)
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Number (fractional, nat, sign)

data Geo = Geo
    { _characteristic :: Int
    , _mantissa :: Int
    } deriving (Eq, Show)

data Node = Node
    { _number    :: Int
    , _latitude  :: Double
    , _longitude :: Double
    } deriving (Eq, Show)

makeLenses ''Geo
makeLenses ''Node

parseNodes :: Parser [Node]
parseNodes = do
    manyTill anyChar $ try $ string "NODE_COORD_SECTION"
    manyTill parseNode $ try $ string "EOF"

parseNode :: Parser Node
parseNode =
    Node <$> (spaces *> nat)
         <*> (spaces *> (sign <*> fractional))
         <*> (spaces *> (sign <*> fractional))
         <*  endOfLine

--parseGeo :: Parser Geo
--parseGeo =
--    Geo <$> (spaces *> (sign <*> nat))
--        <*> (string "." *> nat)

distance :: Node -> Node -> Int
distance i j = floor (rrr' * acos (0.5 * ((1 + q1) * q2 - (1 - q1) * q3)) + 1)
    where
    rrr', q1, q2, q3, latI, latJ, longI, longJ :: Double
    rrr' = _Sphere # rrr
    q1 = cos $ longI - longJ -- 0.9953121781612654
    q2 = cos $ latI - latJ   -- 0.9996341006403823
    q3 = cos $ latI + latJ   -- 0.20193268514150065
    radians node d m = pi' * (node^.d + 5 * node^.m / 3) / 180
    latI = traceShowId $ radians i latD latM -- 0.6702064327658225
    latJ = traceShowId $ radians j latD latM -- 0.6972590361717347
    longI = traceShowId $ radians i longD longM -- 0.361283080000000090
    longJ = traceShowId $ radians j longD longM -- 0.458148833333333283
    latD = latitude.to (fromIntegral . round')
    latM = latitude.to (\x -> x - fromIntegral (round' x))
    longD = longitude.to (fromIntegral . round')
    longM = longitude.to (\x -> x - fromIntegral (round' x))
    round' :: Double -> Int
    round' x = floor (x + 0.5)
    pi' = 3.141592

rrr :: Sphere
rrr = (6378.388 :: Double)^._Sphere

tour :: [Node] -> [(Node, Node)]
tour xs = zip xs (t ++ h)
    where
    (h, t) = splitAt 1 xs

tourDistance :: [(Node, Node)] -> Int
tourDistance = sum . fmap (uncurry distance)

main :: IO ()
main = do
    nodes <- parseFromFile parseNodes "ulysses22.tsp"
    --either print (putStrLn . groom) nodes
    either print (putStrLn . groom . take 1 . tour) nodes
    either print (putStrLn . groom . tourDistance . take 1 . tour) nodes
    --either print (putStrLn . groom . tourDistance . tour . optimalTour) nodes
    --either print (putStrLn . groom . tourDistance . tour) nodes

optimalNodes :: [Int]
optimalNodes = [1, 14, 13, 12, 7, 6, 15, 5, 11, 9, 10, 19, 20, 21, 16, 3, 2, 17, 22, 4, 18, 8]

optimalTour :: [Node] -> [Node]
optimalTour ns = mapMaybe look optimalNodes
    where
    look o = find ((== o) . _number) ns
