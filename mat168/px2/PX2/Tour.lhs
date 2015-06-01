We run the tours here.

> {-# LANGUAGE DeriveTraversable #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE OverloadedLists #-}
> {-# LANGUAGE TypeFamilies #-}
> module PX2.Tour where
>
> import Control.Lens
> import Control.Applicative (Alternative)
> import Control.Monad (MonadPlus)
>
> import Data.Foldable (find)
> import Data.Traversable (for)
> import Data.Tuple (swap)
>
> import GHC.Exts (IsList(..))
>
> import PX2.Distance

We need some way to express tours,
a [Int] might work, but let's give it a newtype just to make things easier.

> newtype Tour a = Tour { unTour :: [a] }
>     deriving ( Alternative, Applicative, Eq, Functor, Foldable, Monad
>              , MonadPlus, Monoid, Ord, Show, Traversable
>              )
> instance IsList (Tour a) where
>     type Item (Tour a) = a
>     fromList = Tour
>     toList = unTour

We can compute the `tourLength` of any list of distances.

> tourLength :: [Distance] -> Tour Int -> Int
> tourLength xs = sum . maybe [] (fmap distance) . tourDistance xs

> tourDistance :: [Distance] -> Tour Int -> Maybe [Distance]
> tourDistance xs ys = for (pair $ toList ys) $ \(i', j') ->
>     find (\d -> i d == min i' j' && j d == max i' j') xs

> pair :: [a] -> [(a, a)]
> pair = zip <*> (uncurry (++) . swap . splitAt 1)

