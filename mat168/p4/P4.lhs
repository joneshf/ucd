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

> import Data.Either (rights)
> import Data.Foldable (for_)
> import Data.List (sort)
>
> import P4.Morphism -- (canonical, farthestInsertion, nearestNeighbors, tourLength, tourDistance)--, twoOpt)
> import P4.Parser (parseData)
>
> import System.FilePath.Glob (glob)
>
> import Text.Parsec.String (parseFromFile)

And off we go.

> main :: IO ()
> main = do
>     -- First glob all the distance file names.
>     tspFiles <- sort <$> glob "distances/*.txt"
>     -- Parse them all to lists of distances.
>     tsps <- mapM (parseFromFile parseData) tspFiles
>     let tsps' = rights tsps
>
>     -- First we compute the canonical tour.
>     for_ (take 1 $ drop 3 tsps') (\xs -> print . tourLength xs . canonical $ xs)
>     -- Next we compute the Nearest Neighbor tour.
>     for_ (take 1 $ drop 3 tsps') (\xs -> print . tourLength xs . nearestNeighbors $ xs)
>     -- Next we compute the Nearest Neighbor followed by twoOpt.
>     --for_ (take 1 $ drop 3 tsps') (\xs -> print . tourLength xs . twoOpt . nearestNeighbors)
>     -- Next we compute the Farthest Insertion tour.
>     --for_ (take 1 $ drop 3 tsps') (\xs -> print . tourLength xs . farthestInsertion)

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
