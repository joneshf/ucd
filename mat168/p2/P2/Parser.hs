module P2.Parser (parseData) where

import Control.Applicative ((<|>), empty)

import P2.Node (EdgeWeightType(..), Node(..), Nodes(..))
import P2.TSPLIB (TSPLIBKeywords(..))

import Text.Parsec ( anyChar, endOfLine, eof, choice, count, manyTill, spaces
                   , string, try
                   )
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Number ( floating2, nat
                                            , sign
                                            )

-- Now we need some way to parse the file.
-- Since we don't have to concern ourselves with the entire format at this time,
-- we do the most simplistic approach possible.

-- First we ignore everything up to grab the `EDGE_WEIGHT_TYPE`
-- so we know what kind of nodes we're dealing with,
-- then ignore everything up until `NODE_COORD_SECTION`,
-- then parse a whole bunch of nodes until `EOF`.

parseData :: Parser Nodes
parseData = do
    dim <- parseDimension
    ewt <- parseEdgeType
    anyChar `manyThen'` NODE_COORD_SECTION
    case ewt of
        Just GEO    -> GEOS    <$> parseNodes dim
        Just EUC_2D -> EUC_2DS <$> parseNodes dim
        Nothing     -> empty

parseNodes :: Int -> Parser [Node s]
parseNodes = (`count` parseNode)

parseDimension :: Parser Int
parseDimension = do
    anyChar `manyThen'` DIMENSION
    spaces *> string ":" *> spaces
    nat

parseEdgeType :: Parser (Maybe EdgeWeightType)
parseEdgeType = do
    anyChar `manyThen'` EDGE_WEIGHT_TYPE
    spaces *> string ":" *> spaces
    choice [ Just GEO <$ try (string' GEO)
           , Just EUC_2D <$ try (string' EUC_2D)
           , pure Nothing
           ]

-- The format for the nodes is:
-- `<integer>` `<real>` `<real>`.

-- We create a parser for this schema.

parseNode :: Parser (Node s)
parseNode =
    Node <$> (spaces *> nat)
         <*> (spaces *> (sign <*> floating2 True))
         <*> (spaces *> (sign <*> floating2 True))
         <*  spaces

manyThen :: Parser a -> Parser b -> Parser [a]
manyThen p q = manyTill p $ try $ q
manyThen' :: Show s => Parser a -> s -> Parser [a]
manyThen' p = manyThen p . string'
string' :: Show s => s -> Parser String
string' = string . show
