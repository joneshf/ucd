We want is to model the data somehow, so we create nodes.

> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE TemplateHaskell #-}
> module P2.Node where

> import Control.Lens (makeLenses)
>
> import Data.Typeable (Typeable)

We want to be able to talk about the different types of input formats.
At the moment we have to handle `EUC_2D` and `GEO`.
Since there's no actual difference between the format of the data,
we can use `Unit` types to represent the different formats.
We promote them to the kind level with `DataKinds`
so that we can talk about which "kind" of node we're dealing with.

> data EdgeWeightType = EUC_2D | GEO deriving (Eq, Show)

Nodes are parameterized by a phantom `EdgeWeightType`.
This is because there's no actual difference between the representation of nodes,
but the way we calculate the distance changes depending on which `EdgeWeightType` we have.

> data Node (s :: EdgeWeightType) = Node
>     { _number :: Int
>     , _r1     :: Double
>     , _r2     :: Double
>     } deriving (Eq, Ord, Show, Typeable)

And of course generate some lenses so we don't end up doing too much work ourselves.

> makeLenses ''Node

We also want a sum of the possible lists of nodes.
This will help when we get to parsing.

> data Nodes = GEOS    [Node 'GEO]
>            | EUC_2DS [Node 'EUC_2D]
