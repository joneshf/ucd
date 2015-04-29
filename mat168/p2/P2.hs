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
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

{-# LANGUAGE RankNTypes #-}

-- Now on to our actual program:

module P2 where

-- We import a whole mess of helper things

import Control.Lens ((^.), _1, _2, _3)

import Data.Foldable (for_)
import Data.Int (Int32)
import Data.List (isInfixOf, sort)

import P2.Distance (Distance(..))
import P2.Morphism -- (incPairsDists, tourDistance)
import P2.Node (Node, Nodes(..))
import P2.Parser (parseData)

import System.FilePath ((</>), replaceExtension, takeFileName)
import System.FilePath.Glob (glob)

import Text.Groom
import Text.Parsec.String (parseFromFile)
import Text.Printf (PrintfType, printf)

-- And off we go.

-- We provide some output formatting

format :: PrintfType a => Int -> Int32 -> a
format = printf
    "The distance of the tour in order 1-2-...-%d-1 is: %d km\n"
format' :: (Distance s, PrintfType a) => [Node s] -> a
format' xs = format (length xs) . tourDistance $ xs

cataNode :: PrintfType a
         => (forall s. (Distance s) => ([Node s] -> a))
         -> Nodes
         -> a
cataNode f (GEOS    ns) = f ns
cataNode f (EUC_2DS ns) = f ns

burma14 :: IO ()
burma14 = do
    putStrLn "Increasing tour of burma14.tsp"
    d <- parseFromFile parseData "all_tsp/burma14.tsp"
    case d of
        Left e -> print e
        Right (GEOS    ns) -> for_ (incPairsDists ns) $ \x ->
            printf "%d %d %d\n" (x^._1) (x^._2) (x^._3)
        Right (EUC_2DS ns) -> for_ (incPairsDists ns) $ \x ->
            printf "%d %d %d\n" (x^._1) (x^._2) (x^._3)

prettyDirect :: FilePath -> Nodes -> IO ()
prettyDirect tsp n = do
    printf "Tour of %s\n" (takeFileName tsp)
    cataNode format' n

prettyInc :: Distance s => [Node s] -> String
prettyInc xs =
    unlines . (len:) . fmap (uncurry3 $ printf "%d %d %d") . incPairsDists $ xs
    where
    len = show $ length xs
    uncurry3 f (x, y, z) = f x y z

writeIncPairs :: Distance s => FilePath -> [Node s] -> IO ()
writeIncPairs fp = writeFile fp . prettyInc

incPairsDir :: FilePath
incPairsDir = "inc_pairs"

incPairsExt :: FilePath
incPairsExt = "inc"

mkIncPairsName :: FilePath -> FilePath
mkIncPairsName fp =
    incPairsDir </> replaceExtension (takeFileName fp) incPairsExt

main :: IO ()
main = do
    -- First glob all the tsp file names.
    tspFiles <- sort <$> glob "all_tsp/*.tsp"
    -- Parse them all and only grab the ones that are valid (`EUC_2D` or `GEO`).
    tsps <- mapM (parseFromFile parseData) tspFiles
    let tsps' = [(fp, ns) | (fp, Right ns) <- zip tspFiles tsps]
    -- We first need to write the increasing distance pairs for each file.
    -- for_ (take 1 tsps') $ \(fp, n) ->
    --     cataNode (writeIncPairs $ mkIncPairsName fp) n
    -- -- Next we want to take compute the direct tour from 1 to n.
    -- for_ tsps' $ uncurry prettyDirect
    -- Now we need to do the nearest neighbor.
    for_ (filter (("ulysses22" `isInfixOf`) . fst) tsps') $ \(fp, n) -> do
        print fp
        case n of
            GEOS    ns -> do
                putStrLn $ groom $ nearestNeighbors ns
                putStrLn $ groom $ nearestDistance ns
            EUC_2DS ns -> do
                putStrLn $ groom $ nearestNeighbors ns
                putStrLn $ groom $ nearestDistance ns
