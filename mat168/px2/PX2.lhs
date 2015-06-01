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
1. Christofides' algorithm:
    1. Find a minimum spanning tree T.
    1. Find a perfect matching M among vertices with odd degree.
    1. Combine the edges of M and T to make a multigraph G.
    1. Find an Euler cycle in G by skipping vertices already seen.
More information is provided in [2].

Again, we first require some pragmas to let us know if something slipped by.

> {-# OPTIONS_GHC -Wall #-}
> {-# OPTIONS_GHC -Werror #-}
> {-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

Now on to our actual program:

> module PX2 where

> import PX2.Algorithm ()

> main :: IO ()
> main = do
>     writeFile "comparison.md" ""
