Programming assignment extra credit 2
=====================================

Hardy Jones
-----------
Professor K&ouml;ppe
--------------------

This program is a series of literate Haskell modules split across multiple files.
So it could be run directly if that were required.


We have our three tasks:

1. The tree shortcut algorithm.
1. The greedy TSP algorithm as an optimization problem over independent systems:
1. Christofides' algorithm.

Here's an explanation of each algorithm.

1. The tree shortcut algorithm first looks at a graph,
and creates a minimum spanning tree (MST).
Then it walks along the MST and adds each nodes seen in preorder.
The shortcut part comes from the preorder traversal.
When walking the tree,
If there's a path `x -> y -> z` and `y` has already been seen,
drop the path through `y` and just take the path `x -> z`.
This works because if `y` is already seen, that means it's already in the tour.
And assuming that the distance between cities is a metric,
the solution is still feasible by the triangle inequality.
It also imposes an upper bound on the solution of being no more than twice the optimal solution.
With a proof and more information given in [1].
1. The greedy algorithm:
    1.  Start with no edges
    1.  Add the edge of minimum distance that does not create a short cycle or
        increase any vertex degree to larger than 2.
    1.  Repeat until a full tour is formed.

    That's pretty much the entirety of the algorithm.
1. Christofides' algorithm.

    1. Find a minimum spanning tree T.
    1. Find a perfect matching M among vertices with odd degree.
    1. Combine the edges of M and T to make a multigraph G.
    1. Find an Euler cycle in G by skipping vertices already seen.
    1. Convert the Euler cycle to a Hamilton cycle.

    More information is provided in [2].

    However, we take a slightly different approach.
    We skip creation of an Euler cycle, and go straight to the Hamilton cycle by shortcutting.
    A proof that this is still a valid implementation of Christofides' algorithm
is provided in [3].

Again, we first require some pragmas to let us know if something slipped by.

> {-# OPTIONS_GHC -Wall #-}
> {-# OPTIONS_GHC -Werror #-}
> {-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

Now on to our actual program:

> module PX2 where

> import Data.Either (rights)
> import Data.List (sort)
>
> import PX2.Algorithm (christofides, greedy, treeShortcut)
> import PX2.Distance (Distance(..), distancesVertices, distancesEdges)
> import PX2.Graph (Graph(..))
> import PX2.Printing (optimalTours, prettyTable)
> import PX2.Parser (parseData)
> import PX2.Tour (Tour(..), tourLength)
>
> import System.FilePath (takeBaseName)
> import System.FilePath.Glob (glob)
>
> import Text.Parsec.String (parseFromFile)

We're only going to run on a few distance files,
since I don't have all day to run these algorithms.

> main :: IO ()
> main = do
>     -- First glob all the distance file names.
>     tspFiles <- sort <$> glob "distances/*.txt"
>     -- Parse them all to lists of distances.
>     tsps <- mapM (parseFromFile parseData) tspFiles
>     let tsps' = rights tsps
>     let algo1 = runAlgorithm treeShortcut <$> tsps'
>     let algo2 = runAlgorithm greedy <$> tsps'
>     let algo3 = runAlgorithm christofides <$> tsps'
>     let files = takeBaseName <$> tspFiles
>     let table = prettyTable [ files
>                             , optimalTours
>                             , fmap show algo1
>                             , fmap show algo2
>                             , fmap show algo3
>                             ]
>     writeFile "comparison.md" table

> distances2Graph :: [Distance] -> Graph Int Int
> distances2Graph = Graph <$> distancesVertices <*> distancesEdges

> runAlgorithm :: (Graph Int Int -> [Int]) -> [Distance] -> Int
> runAlgorithm f ds = tourLength ds $ Tour $ f $ distances2Graph ds

Looking at the comparison between the algorithms.
Neither the tree shortcut nor Christofides' algorithm seem to give enough
of an increase over the greedy version to warrant the amount of effort that goes into them.
Christofides' actually seems to give pretty bad tours.

If we look at the implementation of `treeShortcut` and `christofides`,
they're both pretty complex.
`greedy` on the other hand is pretty understandable,
and makes for a decent tour.

