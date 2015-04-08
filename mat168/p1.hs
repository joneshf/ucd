{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module P1 where

import Control.Applicative ((<$>), (<*>), (<*))

import Data.Char (isUpper)
import Data.Foldable (foldl')

import Text.Parsec (anyChar, char, choice, digit, endOfLine, many1, manyTill, satisfy, spaces, string, try)
import Text.Parsec.String (Parser, parseFromFile)

data Node = Node
    { number :: Int
    , x      :: Float
    , y      :: Float
    } deriving (Eq, Show)

parseNodes :: Parser [Node]
parseNodes = do
    manyTill anyChar (try (string "NODE_COORD_SECTION"))
    manyTill parseNode (try (satisfy isUpper))

parseNode :: Parser Node
parseNode =
    Node <$> (spaces >> read <$> many1 digit)
         <*> parseFloat
         <*> parseFloat
         <*  endOfLine

parseFloat :: Parser Float
parseFloat = spaces >> choice [pos, neg, float]
    where
    pos = char '+' >> float
    neg = char '-' >> negate <$> float
    float = do
        characteristic <- many1 digit
        char '.'
        mantissa <- many1 digit
        return $ read $ characteristic ++ "." ++ mantissa

distance :: Node -> Node -> Float
distance a b = sqrt $ xx * yy
    where
    xx = (x a - x b) * (x a - x b)
    yy = (y a - y b) * (y a - y b)

tour :: [Node] -> [(Node, Node)]
tour xs = zip xs rot
    where
    rot = t ++ h
    (h, t) = splitAt 1 xs

tourDistance :: [(Node, Node)] -> Float
tourDistance = foldl' (\acc (a, b) -> acc + distance a b) 0

main :: IO ()
main = do
    nodes <- parseFromFile parseNodes "ulysses22.tsp"
    either print (print . tourDistance . tour) nodes
