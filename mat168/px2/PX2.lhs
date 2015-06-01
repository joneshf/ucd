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
    1.  Start with no edges
    1.  Add the edge of minimum distance that does not create a short cycle or
        increase any vertex degree to larger than 2.
    1.  Repeat until a full tour is formed
1. Christofides' algorithm.

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
