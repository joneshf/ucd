We want to talk abstractly about the different distances.
Since they (so far) seem to have the same types,
we abstract over them as a typeclass.

> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE KindSignatures #-}
> module P2.Distance (Distance(..)) where

> import Control.Lens ((^.), to)
>
> import Data.Int (Int32)
>
> import P2.Node (EdgeWeightType(..), Node(..), r1, r2)

For each tagged type, we need a different distance function.
Since this is the only thing that changes in the whole algorithm,
we abstract over it with a typeclass.

> class Distance (s :: EdgeWeightType) where
>     distance :: Node s -> Node s -> Int32

Then we can provide an implementation for each `Node` synonym.

The algorithm given in the TSPLIB pdf.
This is basically a direct copy.
Any other implementation ends up with slightly different values.

> instance Distance 'GEO where
>     distance i j = floor $ rrr * acos (((1 + q1) * q2 - (1 - q1) * q3) / 2) + 1
>         where
>         rrr, q1, q2, q3, latI, latJ, longI, longJ, pi' :: Double
>         q1 = cos $ longI - longJ
>         q2 = cos $ latI - latJ
>         q3 = cos $ latI + latJ
>         latI = radians i latDegrees latMinutes
>         latJ = radians j latDegrees latMinutes
>         longI = radians i longDegrees longMinutes
>         longJ = radians j longDegrees longMinutes
>         radians node d m = pi' * (node^.d + 5 * node^.m / 3) / 180
>         latDegrees = r1.to (fromInteger . round)
>         latMinutes = r1.to ((-) <*> fromInteger . round)
>         longDegrees = r2.to (fromInteger . round)
>         longMinutes = r2.to ((-) <*> fromInteger . round)
>         pi' = 3.141592
>         rrr = 6378.388

Euclidean distance on nodes.

> instance Distance 'EUC_2D where
>     distance i j = round $ sqrt $ dx ** 2 + dy ** 2
>         where
>         dx = i^.r1 - j^.r1
>         dy = i^.r2 - j^.r2
