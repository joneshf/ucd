{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module P1 where

import Control.Lens ((^.), (#), makeLenses, to)

import Data.Geo.Geodetic (Sphere, _Sphere)
import Data.Tuple (swap)

import Text.Parsec (anyChar, endOfLine, manyTill, spaces, string, try)
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Number (fractional, nat, sign)

data Node = Node
    { _number    :: Int
    , _latitude  :: Double
    , _longitude :: Double
    } deriving (Eq, Show)

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

distance :: Node -> Node -> Int
distance i j = floor (rrr' * acos (0.5 * ((1 + q1) * q2 - (1 - q1) * q3)) + 1)
    where
    rrr', q1, q2, q3, latI, latJ, longI, longJ, pi' :: Double
    rrr' = _Sphere # rrr
    q1 = cos $ longI - longJ
    q2 = cos $ latI - latJ
    q3 = cos $ latI + latJ
    latI = radians i latDegrees latMinutes
    latJ = radians j latDegrees latMinutes
    longI = radians i longDegrees longMinutes
    longJ = radians j longDegrees longMinutes
    radians node d m = pi' * (node^.d + 5 * node^.m / 3) / 180
    latDegrees = latitude.to (fromInteger . round)
    latMinutes = latitude.to ((-) <*> fromInteger . round)
    longDegrees = longitude.to (fromInteger . round)
    longMinutes = longitude.to ((-) <*> fromInteger . round)
    pi' = 3.141592

rrr :: Sphere
rrr = (6378.388 :: Double)^._Sphere

tour :: [Node] -> [(Node, Node)]
tour = zip <*> (uncurry (++) . swap . splitAt 1)

tourDistance :: [(Node, Node)] -> Int
tourDistance = sum . fmap (uncurry distance)

main :: IO ()
main = do
    nodes <- parseFromFile parseNodes "ulysses22.tsp"
    either print (print . tourDistance . tour) nodes
