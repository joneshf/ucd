Since the graph and tree packages on hackage are pretty much terrible,
we roll our own graph.

> {-# LANGUAGE DeriveAnyClass #-}
> {-# LANGUAGE DeriveDataTypeable #-}
> {-# LANGUAGE DeriveFoldable #-}
> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE TupleSections #-}
> module PX2.Graph where

> import Control.Lens
>
> import Data.Data
> import Data.Traversable (for)
>
> import GHC.Generics
>
> import Test.QuickCheck.Arbitrary
>
> import qualified Data.Set as S

> data Graph v w = Graph
>     { vertices :: S.Set v
>     , edges    :: S.Set (v, v, w)
>     } deriving (Data, Eq, Foldable, Generic, Ord, Show, Typeable)
> type MST v w = S.Set (v, v, w)
>
> instance (Arbitrary v, Arbitrary w, Ord v, Ord w) => Arbitrary (Graph v w) where
>     arbitrary = do
>         vs <- S.fromList <$> arbitrary
>         let es = [(i, j) | i <- S.toList vs, j <- S.toList vs, i < j]
>         es' <- for es $ \(i, j) -> (i, j, ) <$> arbitrary
>         pure $ Graph vs (S.fromList es')

> unweightedEdges :: Ord v => S.Set (v, v, w) -> S.Set (v, v)
> unweightedEdges =
>     S.fromList . (map $ (,) <$> view _1 <*> view _2) . S.toList

> cartesian :: (Ord a, Ord b) => S.Set a -> S.Set b -> S.Set (a, b)
> cartesian xs ys = S.fromList $ (,) <$> S.toList xs <*> S.toList ys
