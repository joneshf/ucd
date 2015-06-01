An orphan instance for `Data.MultiSet`.

> {-# LANGUAGE TypeFamilies #-}
> module PX2.Orphan where

> import GHC.Exts (IsList(..))
>
> import qualified Data.MultiSet as MS

> instance Ord a => IsList (MS.MultiSet a) where
>     type Item (MS.MultiSet a) = a
>     fromList = MS.fromList
>     toList = MS.toList
