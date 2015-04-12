{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module P1 where

import Control.Lens

import Data.Char (digitToInt,isUpper)

import Text.Parsec (anyChar, chainl1, char, choice, digit, endOfLine, manyTill, satisfy, spaces, string, try)
import Text.Parsec.String (Parser, parseFromFile)

newtype Degree = Degree { _runDegree :: Int }
    deriving (Enum, Eq, Integral, Num, Ord, Real, Show)
newtype Minute = Minute { _runMinute :: Int }
    deriving (Enum, Eq, Integral, Num, Ord, Real, Show)

data Geo = Geo
    { _degree :: Degree
    , _minute :: Minute
    } deriving (Eq, Show)

data Node = Node
    { _number    :: Int
    , _latitude  :: Geo
    , _longitude :: Geo
    } deriving (Eq, Show)

makeLenses ''Node
makeLenses ''Geo
makeLenses ''Degree
makeLenses ''Minute

parseNodes :: Parser [Node]
parseNodes = do
    manyTill anyChar $ try $ string "NODE_COORD_SECTION"
    manyTill parseNode $ try $ satisfy isUpper

parseNode :: Parser Node
parseNode =
    Node <$> (spaces *> parseInt)
         <*> parseGeo
         <*> parseGeo
         <*  endOfLine

parseGeo :: Parser Geo
parseGeo = spaces *> choice [pos, neg, geo]
    where
    pos = char '+' *> geo
    neg = char '-' *> geo
    geo =
        Geo <$> (Degree <$> parseInt) <* char '.'
            <*> (Minute <$> parseInt)

parseInt :: Parser Int
parseInt = chainl1 (digitToInt <$> digit) (pure $ (+) . (* 10))

distance :: Node -> Node -> Float
distance a b = sqrt $ fromIntegral ((((a^.latitude.degree - b^.latitude.degree)^(2 :: Int) * (a^.longitude.degree - b^.longitude.degree)^(2 :: Int)))^.runDegree)
--distance _ _ = 1
--distance a b = sqrt $ xx * yy
--    where
--    xx = (runGeo (latitude a) - runGeo (latitude b)) * (runGeo (latitude a) - runGeo (latitude b))
--    yy = (runGeo (longitude a) - runGeo (longitude b)) * (runGeo (longitude a) - runGeo (longitude b))

tour :: [Node] -> [(Node, Node)]
tour xs = zip xs rot
    where
    rot = t ++ h
    (h, t) = splitAt 1 xs

tourDistance :: [(Node, Node)] -> Float
tourDistance = sum . fmap (uncurry distance)

main :: IO ()
main = do
    nodes <- parseFromFile parseNodes "ulysses22.tsp"
    either print (print . tourDistance . tour) nodes
