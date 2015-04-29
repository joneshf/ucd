-- Programming assignment 2
-- ========================

-- Hardy Jones
-- -----------
-- 999397426
-- ---------
-- Professor K&ouml;ppe
-- --------------------

-- This file is a literate haskell file.
-- So it could be run directly if that were required.

-- We only have to make slight modifications to our previous program in order to handle `EUC_2D`and `GEO` files.

-- Again, we first require some pragmas to let us know if something slipped by.

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

{-# LANGUAGE RankNTypes #-}

-- Now on to our actual program:

module P2 where

-- We import a whole mess of helper things

import Control.Lens ((^.), _1, _2, _3)

import Data.Foldable (for_)
import Data.List (sort)

import P2.Distance (Distance(..))
import P2.Morphism (incPairsDists, tourDistance)
import P2.Node (Node, Nodes(..))
import P2.Parser (parseData)

import System.FilePath (takeFileName)
import System.FilePath.Glob (glob)

import Text.Parsec.String (parseFromFile)
import Text.Printf (PrintfType, printf)

-- And off we go.

-- We provide some output formatting

format :: PrintfType a => Int -> Int -> a
format = printf
    "The distance of the tour in order 1-2-...-%d-1 is: %d km\n"
format' :: (Distance s, PrintfType a) => [Node s] -> a
format' xs = format (length xs) . tourDistance $ xs

cataNode :: PrintfType a => (forall s. (Distance s) => ([Node s] -> a)) -> Nodes -> a
cataNode f (GEOS    ns) = f ns
cataNode f (EUC_2DS ns) = f ns

burma14 :: IO ()
burma14 = do
    putStrLn "Increasing tour of burma14.tsp"
    d <- parseFromFile parseData "all_tsp/burma14.tsp"
    case d of
        Left e -> print e
        Right (GEOS    ns) -> for_ (incPairsDists ns) $ \x ->
            printf "%d %d %d\n" (x^._1) (x^._2) (x^._3 :: Int)
        Right (EUC_2DS ns) -> for_ (incPairsDists ns) $ \x ->
            printf "%d %d %d\n" (x^._1) (x^._2) (x^._3 :: Int)

pretty :: FilePath -> Either a Nodes -> IO ()
pretty tsp (Right n) = do
    printf "Tour of %s\n" (takeFileName tsp)
    cataNode format' n
-- We just silently ignore parse errors,
pretty _    _        = pure ()

-- Finally, we actually attempt to run this program.
-- First parse the file.
-- If it doesn't parse, then print out the message and be done.
-- If it parses fine, then create a tour, find the distance, and print it.

main :: IO ()
main = do
    tsps <- sort <$> glob "all_tsp/*.tsp"
    for_ tsps $ \tsp -> parseFromFile parseData tsp >>= pretty tsp

-- We can see this in action by running on the terminal:
