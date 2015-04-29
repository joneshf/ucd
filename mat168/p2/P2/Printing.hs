{-# LANGUAGE RankNTypes #-}
module P2.Printing where

import Control.Lens (view)

import Data.Int (Int32)

import P2.Distance (Distance)
import P2.Morphism (incPairsDists, nearestNeighbors, tourDistance)
import P2.Node (Node, Nodes(..), number)

import System.FilePath ((</>), replaceExtension, takeBaseName, takeFileName)

import Text.Printf (PrintfType, printf)

cataNode :: PrintfType a
         => (forall s. (Distance s) => ([Node s] -> a))
         -> Nodes
         -> a
cataNode f (GEOS    ns) = f ns
cataNode f (EUC_2DS ns) = f ns

-- We provide some output formatting

prettyDirect :: FilePath -> Nodes -> IO ()
prettyDirect tsp n = do
    printf "Tour of %s\n" (takeFileName tsp)
    cataNode direct' n

direct :: PrintfType a => Int -> Int32 -> a
direct = printf
    "The distance of the tour in order 1-2-...-%d-1 is: %d km\n"

direct' :: (Distance s, PrintfType a) => [Node s] -> a
direct' xs = direct (length xs) . tourDistance $ xs

prettyInc :: Distance s => [Node s] -> String
prettyInc xs =
    unlines . (len:) . fmap (uncurry3 $ printf "%d %d %d") . incPairsDists $ xs
    where
    len = show $ length xs
    uncurry3 f (x, y, z) = f x y z

prettyIncPairs :: FilePath -> Nodes -> IO ()
prettyIncPairs = writeIncPairs . mkIncPairsName

writeIncPairs :: FilePath -> Nodes -> IO ()
writeIncPairs fp = cataNode (writeFile fp . prettyInc)

incPairsDir :: FilePath
incPairsDir = "inc_pairs"

incPairsExt :: FilePath
incPairsExt = "inc"

mkIncPairsName :: FilePath -> FilePath
mkIncPairsName fp =
    incPairsDir </> replaceExtension (takeFileName fp) incPairsExt

prettyNearest :: FilePath -> Nodes -> IO ()
prettyNearest fp = cataNode (prettyNearest' fp)

prettyNearest' :: Distance s => FilePath -> [Node s] -> IO ()
prettyNearest' fp ns = do
    let nearest = nearestNeighbors ns
    printf "Tour of %s:\n" $ takeBaseName fp
    print $ fmap (view number) nearest
    printf "Distance of tour: %d\n\n" $ tourDistance nearest
