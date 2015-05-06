With the following module,
we'd like to gain some confidence that we've done things correctly.

Not everything is tested.
There's only so much time in the day, and this is only worth 20 points.
But we can throw together a couple of quick properties.

> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE FlexibleInstances #-}
> module P2.Test where

Again, we import a bunch of stuff.

> import P2.Node (EdgeWeightType(..), Node(..))
> import P2.Distance (Distance(..))
> import P2.Morphism (tour, tourDistance)

> import Data.Tuple (swap)

> import Test.QuickCheck ( Arbitrary(..), Args(..), Property, Testable, (.&&.), choose
>                        , quickCheck, quickCheckWith, stdArgs
>                        )

Now we make an instance for generating arbitrary `Node GEO`s.

> instance Arbitrary (Node 'GEO) where
>     arbitrary = Node <$> arbitrary <*> choose (-90, 90) <*> choose (-180, 180)

And an instance for generating arbitrary `Node EUC_2D`s.

> instance Arbitrary (Node 'EUC_2D) where
>     arbitrary = Node <$> arbitrary <*> arbitrary <*> arbitrary

And we'd like the following properties to be true:

We'd like for `distance` to be a metric.
However, it does not have to satisfy reflexivity for all types.
We still create a prop for the distances that *should* be metrics.

1: `distance x x == 0`

> prop_ReflexiveDistance :: Distance s => Node s -> Bool
> prop_ReflexiveDistance x = distance x x == 0

2: `distance x y == distance y x`

> prop_SymmetricDistance :: Distance s => Node s -> Node s -> Bool
> prop_SymmetricDistance x y = distance x y == distance y x

Non-negativity

3: `distance x y >= 0`

> prop_NonNegativeDistance :: Distance s => Node s -> Node s -> Bool
> prop_NonNegativeDistance x y = distance x y >= 0

Triangle inequality

4: `distance x z <= distance x y + distance y z`

> prop_Triangle :: Distance s => Node s -> Node s -> Node s -> Bool
> prop_Triangle x y z = distance x z <= distance x y + distance y z

Metric

We can wrap up all these properties into one metric prop,
so we don't have to test so much stuff explicitly.

5: Properties 1-4

> prop_MetricDistance :: Distance s => Node s -> Node s -> Node s -> Property
> prop_MetricDistance x y z = prop_ReflexiveDistance x
>                        .&&. prop_SymmetricDistance x y
>                        .&&. prop_NonNegativeDistance x y
>                        .&&. prop_Triangle x y z

6: `tour xs++tour xs == tour (xs++xs)`

> prop_ConcatTour :: Distance s => [Node s] -> Bool
> prop_ConcatTour xs = tour xs ++ tour xs == tour (xs ++ xs)

7: `tourDistance (xs++ys) == tourDistance (ys++xs)`

This property is a bit hard to understand,
What it says is that if we have two parts of the same tour,
then the distance of each part commutes.

> prop_CommutativeTourDistance :: Distance s => [Node s] -> [Node s] -> Bool
> prop_CommutativeTourDistance xs ys =
>     tourDistance (xs' ++ ys') == tourDistance (ys' ++ xs')
>     where
>     xs' = tour xs
>     ys' = tour ys

8: `tourDistance xs+tourDistance ys == tourDistance (xs++ys)`

Here we have that `tourDistance` is a Semigroup Homomorphism
from ([(Node s, Node s)], ++) to (N, +).
Unfortunately, since `distance` is not a metric in general,
it cannot be more powerful than that.

> prop_HomomorphismTourDistance :: Distance s => [Node s] -> [Node s] -> Bool
> prop_HomomorphismTourDistance xs ys =
>     tourDistance xs' + tourDistance ys' == tourDistance (xs' ++ ys')
>     where
>     xs' = tour xs
>     ys' = tour ys

For TSP we should be able to show that it is actually symmetric.

9: `tourDistance (tour xs) == tourDistance (tour (reverse xs))`

> prop_SymmetricTSP :: Distance s => [Node s] -> Bool
> prop_SymmetricTSP xs =
>     tourDistance (tour xs) == tourDistance (tour $ reverse xs)

10: `tourDistance xs == tourDistance (shift xs)`

And we should also be able to show that
if we start the tour at a different point,
the distance does not change.
E.g. Instead of 1-2-...-22-1 we have 2-3-...-22-1-2.

> prop_ShiftTSP :: Distance s => [Node s] -> Int -> Bool
> prop_ShiftTSP xs n = tourDistance (tour xs) == tourDistance (tour $ shift xs)
>     where
>     n' = n `mod` (length xs + 1)
>     shift = uncurry (++) . swap . splitAt n'

Monomorphize `quickCheck` so we don't have to annotate each call.

We can do most of the testing with `GEO`.
Since most of the functions are pure and polymorphic,
they can't actually depend on much implementation details.
We just have to monomorphize the `quickCheck` function
so it knows what instance to magic up.

Unfortunately, that means we need one for things that start with `Node s`,
and another for things that start with `[Node s]`, etc.

> quickCheckGEO :: Testable prop => (Node GEO -> prop) -> IO ()
> quickCheckGEO = quickCheck
> quickCheckGEOs :: Testable prop => ([Node GEO] -> prop) -> IO ()
> quickCheckGEOs = quickCheck
> quickCheckEUC_2D :: Testable prop => (Node EUC_2D -> prop) -> IO ()
> quickCheckEUC_2D = quickCheck

Here we begin running our properties.

> main :: IO ()
> main = do
>     -- First we test most of the stuff with `Node GEO`.
>     quickCheckGEO prop_NonNegativeDistance
>     quickCheckGEO prop_SymmetricDistance
>     quickCheckGEO prop_Triangle
>     quickCheckGEOs prop_CommutativeTourDistance
>     quickCheckGEOs prop_ConcatTour
>     quickCheckGEOs prop_HomomorphismTourDistance
>     quickCheckGEOs prop_ShiftTSP
>     quickCheckGEOs prop_SymmetricTSP
>     -- Now we check stuff specific to `Node EUC_2D`.
>     quickCheckEUC_2D prop_MetricDistance

Finally, we have a few unit tests we'd like to have as well.
For the sake of not making this module any larger,
we simply run quickCheck once.

The tour constructed from the empty list of nodes, should be empty.

11: `tour [] == []`

>     quickCheckWith stdArgs {maxSuccess = 1} (tour ([] :: [Node GEO]) == [])

The distance of no tour should be 0

12: `tourDistance [] == 0`

>     quickCheckWith stdArgs {maxSuccess = 1} (tourDistance ([] :: [(Node GEO, Node GEO)]) == 0)

Half of the circumference of The Earth should be 20039 km.

13: `distance (Node _ 0 0) (Node _ 0 180) == 20039`

>     quickCheckWith stdArgs {maxSuccess = 1}
>         (distance (Node 1 0 0 :: Node 'GEO) (Node 2 0 180) == 20039)

A final sanity check is that
the distance between the first two nodes should be 492 km.

>     quickCheckWith stdArgs {maxSuccess = 1}
>         (distance (Node 1 38.24 20.42 :: Node 'GEO) (Node 2 39.57 26.15) == 492)

Having all of these properties hold
gives us some confidence that we've constructed a valid program.

We can see this in action by running on the terminal:

![Testing](images/test.png)
