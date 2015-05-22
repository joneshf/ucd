We want to represent the distance as
a pair of node numbers and the distance between them.

> {-# LANGUAGE TemplateHaskell #-}
> module P4.Distance where
>
> import Control.Lens (makeLenses)
>
> data Distance = Distance
>     { _i        :: Int
>     , _j        :: Int
>     , _distance :: Int
>     } deriving (Eq, Ord, Show)

We use a little lens magic to make things easy on us.

> makeLenses ''Distance
