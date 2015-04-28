{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
module P2.Node where

import Control.Lens (makeLenses)

import Data.Typeable (Typeable)

-- Next, we want to be able to talk about the different types of input formats.
-- At the moment we have to handle `EUC_2D` and `GEO`.
-- Since there's no actual difference between the format of the data,
-- we can use void types to represent the different formats.
-- The reason being, we don't need to have values of these types,
-- just tag `Node`s with them.

data EdgeWeightType = EUC_2D | GEO deriving (Eq, Show)

-- The first thing we want is to model the data somehow.
-- It'd be much more ideal to use `Data.Geo.Coordinate`,
-- however, it uses a slightly different algorithm for computing the distance,
-- and the difference in roundoff error is too great.

-- That said, we just model it directly as given:

data Node (s :: EdgeWeightType) = Node
    { _number :: Int
    , _r1     :: Double
    , _r2     :: Double
    } deriving (Eq, Ord, Show, Typeable)

-- And of course generate some lenses so we don't end up doing too much work ourselves.

makeLenses ''Node

-- We also want a sum of the possible lists of nodes.

data Nodes = GEOS    [Node 'GEO]
           | EUC_2DS [Node 'EUC_2D]
