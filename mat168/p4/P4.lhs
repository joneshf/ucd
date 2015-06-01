Programming assignment 4
========================

Hardy Jones
-----------
Professor K&ouml;ppe
--------------------

This program is a series of literate Haskell modules split across multiple files.
So it could be run directly if that were required.

We can use most of our previous work.

Again, we first require some pragmas to let us know if something slipped by.

> {-# OPTIONS_GHC -Wall #-}
> {-# OPTIONS_GHC -Werror #-}
> {-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

Now on to our actual program:

> module P4 where

We import a whole mess of helper things

> import Control.Lens
>
> import Data.Either (rights)
> import Data.List (sort)
>
> import P4.Morphism
> import P4.Parser (parseData)
>
> import System.FilePath (takeBaseName)
> import System.FilePath.Glob (glob)
>
> import Text.Parsec.String (parseFromFile)
> import Text.PrettyPrint.Boxes

And off we go.

Unfortunately, I didn't plan time acccordingly, and do not have enough time to run all results.
Only burma14 and the ulysses tours are run

> main :: IO ()
> main = do
>     -- First glob all the distance file names.
>     tspFiles <- sort <$> glob "distances/*.txt"
>     let tspFiles' = take 1 (drop 3 tspFiles) ++ drop (length tspFiles - 2) tspFiles
>     let opts = take 1 (drop 3 optimalTours) ++ drop (length optimalTours - 2) optimalTours
>     -- Parse them all to lists of distances.
>     tsps <- mapM (parseFromFile parseData) tspFiles'
>     let tsps' = rights tsps
>
>     -- First we compute the canonical tour.
>     let d1 = tsps' <&> (\xs -> tourLength xs . canonical $ xs)
>     -- Next we compute the Nearest Neighbor tour.
>     let d2 = tsps' <&> (\xs -> tourLength xs . nearestNeighbors $ xs)
>     -- Next we compute the Nearest Neighbor followed by twoOpt.
>     let d3 = tsps' <&> (\xs -> tourLength xs . twoOpt xs . nearestNeighbors $ xs)
>     -- Next we compute the Nearest Neighbor followed by lin'Kernighan.
>     let d4 = tsps' <&> (\xs -> tourLength xs . lin'Kernighan xs . nearestNeighbors $ xs)
>     -- Next we compute the Farthest Insertion tour.
>     let d5 = tsps' <&> (\xs -> tourLength xs . farthestInsertion $ xs)
>     -- Next we compute the Farthest Insertion followed by twoOpt.
>     let d6 = tsps' <&> (\xs -> tourLength xs . twoOpt xs . farthestInsertion $ xs)
>     -- Next we compute the Farthest Insertion followed by lin'Kernighan.
>     let d7 = tsps' <&> (\xs -> tourLength xs . lin'Kernighan xs . farthestInsertion $ xs)
>     let table = prettyTable [ takeBaseName <$> tspFiles'
>                             ,                  opts
>                             , show         <$> d1
>                             , show         <$> d2
>                             , show         <$> d3
>                             , show         <$> d4
>                             , show         <$> d5
>                             , show         <$> d6
>                             , show         <$> d7
>                             ]
>     writeFile "comparison.md" table

> optimalTours :: [String]
> optimalTours =
>     [ "2579"
>     , "202310"
>     , "7542"
>     , "3323"
>     , "69853"
>     , "40160"
>     , "134602"
>     , "171414"
>     , "294358"
>     , "55209"
>     , "80369"
>     , "36905"
>     , "6859"
>     , "7013"
>     ]

> prettyTable :: [[String]] -> String
> prettyTable = prettyCols
>
> -- The `boxes` api isn't so great to use, but quite powerful.
> prettyCols :: [[String]] -> String
> prettyCols = render . hsep 2 left . mkHeaders . map (vcat left . map text)
>     where
>     mkHeaders = zipWith mkHeaders' headers
>     mkHeaders' h c = h // separator (max (cols h) (cols c)) // c
>     separator n = text (replicate n '-')
>     headers = map text [ "Filename"
>                        , "Optimal"
>                        , "Canonical"
>                        , "NN"
>                        , "NN 2-opt"
>                        , "NN LK"
>                        , "FI"
>                        , "FI 2-opt"
>                        , "FI LK"
>                        ]

We can compare the distances.
Notice that sometimes nearest neighbors can return a worse distance than even the na&iuml;ve straight line tour.
But it's also never better than the optimal.
