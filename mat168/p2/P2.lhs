Programming assignment 2
========================

Hardy Jones
-----------
999397426
---------
Professor K&ouml;ppe
--------------------

This program is a series of literate Haskell modules split across multiple files.
So it could be run directly if that were required.

We only have to make slight modifications to our previous program in order to handle `EUC_2D`and `GEO` files.

Again, we first require some pragmas to let us know if something slipped by.

> {-# OPTIONS_GHC -Wall #-}
> {-# OPTIONS_GHC -Werror #-}
> {-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

Now on to our actual program:

> module P2 where

We import a whole mess of helper things

> import Data.Foldable (for_)
> import Data.List (sort)
>
> import P2.Parser (parseData)
> import P2.Printing (prettyDirect, prettyIncPairs, prettyNearest, prettyTable)
>
> import System.FilePath.Glob (glob)
>
> import Text.Parsec.String (parseFromFile)

And off we go.

> main :: IO ()
> main = do
>     -- First glob all the tsp file names.
>     tspFiles <- sort <$> glob "all_tsp/*.tsp"
>     -- Parse them all and only grab the ones that are valid (`EUC_2D` or `GEO`).
>     tsps <- mapM (parseFromFile parseData) tspFiles
>     let tsps' = [(fp, ns) | (fp, Right ns) <- zip tspFiles tsps]
>     -- We first need to write the increasing distance pairs for each file.
>     for_ tsps' $ uncurry prettyIncPairs
>     -- Next we want to take compute the direct tour from 1 to n.
>     for_ tsps' $ uncurry prettyDirect
>     -- Now we need to do the nearest neighbor.
>     for_ tsps' $ uncurry prettyNearest
>     writeFile "comparison.md" $ prettyTable tsps'

We can see the result of running this program.

First the direct tour.

![Screenshot of direct tour](images/direct.png)

Then the nearest neighbor tour.

![Screenshot of nearest tour](images/nearest.png)

And finally the time of the whole program.

![Screenshot of total time](images/time.png)

We can compare the distances.
Notice that sometimes nearest neighbors can return a worse distance than even the na&iuml;ve straight line tour.
But it's also never better than the optimal.
