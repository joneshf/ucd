Here we want to have different ways of printing the nodes.
This is kind of a conglomeration of `IO a` functions and evaluations.
Not a whole lot of purity in here.

> {-# LANGUAGE RankNTypes #-}
> module P2.Printing where

> import Control.Lens (view)
>
> import Data.Foldable (foldl')
> import Data.Int (Int32)
> import Data.List (transpose)
>
> import P2.Distance (Distance)
> import P2.Morphism (incPairsDists, nearestDistance, nearestNeighbors, tourDistance)
> import P2.Node (Node, Nodes(..), number)
> import P2.Table (augment)
>
> import System.FilePath ((</>), replaceExtension, takeBaseName, takeFileName)
>
> import Text.Printf (PrintfType, printf)
> import Text.PrettyPrint.Boxes
>
> import qualified Data.Map as M

First, make a helper catamorphism that unwraps the `Nodes`.

> cataNode :: PrintfType a
>          => (forall s. (Distance s) => ([Node s] -> a))
>          -> Nodes
>          -> a
> cataNode f (GEOS    ns) = f ns
> cataNode f (EUC_2DS ns) = f ns

> cataNode' :: (forall s. (Distance s) => ([Node s] -> a))
>           -> Nodes
>           -> a
> cataNode' f (GEOS    ns) = f ns
> cataNode' f (EUC_2DS ns) = f ns

We can print a list of nodes directly.

> prettyDirect :: FilePath -> Nodes -> IO ()
> prettyDirect tsp n = do
>     printf "Tour of %s\n" (takeFileName tsp)
>     cataNode direct' n
>
> direct :: PrintfType a => Int -> Int32 -> a
> direct = printf
>     "The distance of the tour in order 1-2-...-%d-1 is: %d km\n"
>
> direct' :: (Distance s, PrintfType a) => [Node s] -> a
> direct' xs = direct (length xs) . tourDistance $ xs

We can construct pairs in increasing order and take the distances.

> prettyIncPairs :: FilePath -> Nodes -> IO ()
> prettyIncPairs = writeIncPairs . mkIncPairsName
>
> writeIncPairs :: FilePath -> Nodes -> IO ()
> writeIncPairs fp = cataNode (writeFile fp . prettyInc)
>
> prettyInc :: Distance s => [Node s] -> String
> prettyInc xs = unlines . (len:) . fmap go . incPairsDists $ xs
>     where
>     go = uncurry3 $ printf "%d %d %d"
>     len = show $ length xs
>     uncurry3 f (x, y, z) = f x y z
>
> mkIncPairsName :: FilePath -> FilePath
> mkIncPairsName fp = incPairsDir </> replaceExtension (takeFileName fp) incPairsExt
>
> incPairsDir :: FilePath
> incPairsDir = "inc_pairs"
>
> incPairsExt :: FilePath
> incPairsExt = "inc"

We can find the nearest neighbors and take the distances.

> prettyNearest :: FilePath -> Nodes -> IO ()
> prettyNearest fp = cataNode (prettyNearest' fp)
>
> prettyNearest' :: Distance s => FilePath -> [Node s] -> IO ()
> prettyNearest' fp ns = do
>     let nearest = nearestNeighbors ns
>     printf "Tour of %s:\n" $ takeBaseName fp
>     print $ fmap (view number) nearest
>     printf "Distance of tour: %d\n\n" $ tourDistance nearest

We want to make things easier on ourselves for creating a table.

> prettyTable :: [(FilePath, Nodes)] -> String
> prettyTable = prettyTable' . augment . allBothDistances
>
> prettyTable' :: M.Map String (Int32, Int32, [Int32]) -> String
> prettyTable' = prettyCols . M.toList
>
> prettyCols :: [(String, (Int32, Int32, [Int32]))] -> String
> prettyCols = render . hsep 2 left . mkHeaders . map (vcat left) . transpose . map go
>     where
>     go (name, (x, y, z)) = [text name, text' x, text' y, text' z]
>     mkHeaders = zipWith mkHeaders' headers
>     mkHeaders' h c = h // separator (max (cols h) (cols c)) // c
>     separator n = text (replicate n '-')
>     headers = map text ["Filename", "Direct", "Nearest Neighbor", "Optimal"]
>     text' :: Show a => a -> Box
>     text' = text . show
>
> allBothDistances :: [(FilePath, Nodes)] -> M.Map String (Int32, Int32)
> allBothDistances =
>     foldl' (\acc (x, y) -> uncurry M.insert (bothDistances x y) acc) M.empty
>
> bothDistances :: FilePath -> Nodes -> (String, (Int32, Int32))
> bothDistances fp = cataNode' (bothDistances' fp)
>
> bothDistances' :: Distance s => FilePath -> [Node s] -> (String, (Int32, Int32))
> bothDistances' fp ns = (takeBaseName fp, (tourDistance ns, nearestDistance ns))
