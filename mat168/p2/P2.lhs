Programming assignment 2
========================

Hardy Jones
-----------
999397426
---------
Professor K&ouml;ppe
--------------------

This file is a literate haskell file.
So it could be run directly if that were required.

We only have to make slight modifications to our previous program in order to handle `EUC_2D`and `GEO` files.

Again, we first require some pragmas to let us know if something slipped by.

> {-# OPTIONS_GHC -Wall #-}
> {-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

And also for lens generation:

> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE TypeSynonymInstances #-}

Now on to our actual program:

> module P2 where

We import a whole mess of helper things

> import Control.Lens -- ((^.), makeLenses, to)
> import Control.Applicative

> import Data.Foldable (for_)
> import Data.List (sort)
> import Data.Tuple (swap)
> import Data.Typeable (Typeable)

> import System.FilePath (takeFileName)
> import System.FilePath.Glob (glob)

> import Text.Parsec ( anyChar, endOfLine, manyTill, spaces
>                    , string, try
>                    )
> import Text.Parsec.String (Parser, parseFromFile)
> import Text.ParserCombinators.Parsec.Number ( floating2, nat
>                                             , sign
>                                             )
> import Text.Printf (PrintfType, printf)

And off we go.

First, since we seem to be parsing the whole file,
let's start encoding the grammar.
We might not use it all at the moment,
but better to have it done now.

> data TSPLIBKeywords = NAME
>                     | TYPE
>                     | COMMENT
>                     | CAPACITY
>                     | DIMENSION
>                     | EDGE_WEIGHT_TYPE
>                     | EDGE_WEIGHT_FORMAT
>                     | EDGE_DATA_FORMAT
>                     | NODE_COORD_TYPE
>                     | DISPLAY_DATA_TYPE
>                     | EOF
>                     | NODE_COORD_SECTION
>                     | DEPOT_SECTION
>                     | DEMAND_SECTION
>                     | EDGE_DATA_SECTION
>                     | FIXED_EDGES_SECTION
>                     | DISPLAY_DATA_SECTION
>                     | TOUR_SECTION
>                     | EDGE_WEIGHT_SECTION
>                     deriving (Eq, Show)

Next, we want to be able to talk about the different types of input formats.
At the moment we have to handle `EUC_2D` and `GEO`.
Since there's no actual difference between the format of the data,
we can use void types to represent the different formats.
The reason being, we don't need to have values of these types,
just tag `Node`s with them.

> data EdgeWeightType = EUC_2D | GEO deriving (Eq, Show)

The first thing we want is to model the data somehow.
It'd be much more ideal to use `Data.Geo.Coordinate`,
however, it uses a slightly different algorithm for computing the distance,
and the difference in roundoff error is too great.

That said, we just model it directly as given:

> data Node (s :: EdgeWeightType) = Node
>     { _number :: Int
>     , _r1     :: Double
>     , _r2     :: Double
>     } deriving (Eq, Ord, Show, Typeable)

And of course generate some lenses so we don't end up doing too much work ourselves.

> makeLenses ''Node

We also want a sum of the possible lists of nodes.

> data Nodes = GEOS [Node 'GEO]
>            | EUC_2DS [Node 'EUC_2D]

For each tagged type, we need a different distance function.
Since this is the only thing that changes in the whole algorithm,
we abstract over it with a typeclass.

> class Distance (s :: EdgeWeightType) where
>     distance :: Integral a => Node s -> Node s -> a

Then we can provide an implementation for each `Node` synonym.

The algorithm given in the TSPLIB pdf.
This is basically a direct copy.
Any other implementation ends up with slightly different values.

> instance Distance 'GEO where
>     distance i j = floor (rrr * acos (((1 + q1) * q2 - (1 - q1) * q3) / 2) + 1)
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

Now we need some way to parse the file.
Since we don't have to concern ourselves with the entire format at this time,
we do the most simplistic approach possible.

First we ignore everything up to grab the `EDGE_WEIGHT_TYPE`
so we know what kind of nodes we're dealing with,
then ignore everything up until `NODE_COORD_SECTION`,
then parse a whole bunch of nodes until `EOF`.

> parseData :: Parser Nodes
> parseData = do
>     ewt <- parseEdgeType
>     anyChar `manyThen'` NODE_COORD_SECTION
>     case ewt of
>         Just GEO    -> GEOS    <$> parseNodes
>         Just EUC_2D -> EUC_2DS <$> parseNodes
>         Nothing     -> empty

> parseNodes :: Parser [Node s]
> parseNodes = parseNode `manyThen'` EOF

> parseEdgeType :: Parser (Maybe EdgeWeightType)
> parseEdgeType = do
>     anyChar `manyThen'` EDGE_WEIGHT_TYPE
>     spaces *> string ":" *> spaces
>     Just GEO <$ try (string' GEO) <|> Just EUC_2D <$ try (string' EUC_2D) <|> pure Nothing

The format for the nodes is:
`<integer>` `<real>` `<real>`.

We create a parser for this schema.

> parseNode :: Parser (Node s)
> parseNode =
>     Node <$> (spaces *> nat)
>          <*> (spaces *> (sign <*> floating2 True))
>          <*> (spaces *> (sign <*> floating2 True))
>          <*  endOfLine

> manyThen :: Parser a -> Parser b -> Parser [a]
> manyThen p q = manyTill p $ try $ q
> manyThen' :: Show s => Parser a -> s -> Parser [a]
> manyThen' p = manyThen p . string'
> string' :: Show s => s -> Parser String
> string' = string . show

We construct a tour by pairing up each node with its immediate successor,
and wrapping the end of the list around.

> tour :: [a] -> [(a, a)]
> tour = zip <*> (uncurry (++) . swap . splitAt 1)

> tourDistance :: Distance s => [(Node s, Node s)] -> Int
> tourDistance = sum . fmap (uncurry distance)

We provide some output formatting

> format :: PrintfType a => Int -> Int -> a
> format = printf
>     "The distance of the tour in order 1-2-...-%d-1 is: %d km\n"
> format' :: (Distance s, PrintfType a) => [Node s] -> a
> format' xs = format (length xs) . tourDistance . tour $ xs

> cataNode :: PrintfType a => (forall s. (Distance s) => ([Node s] -> a)) -> Nodes -> a
> cataNode f (GEOS    ns) = f ns
> cataNode f (EUC_2DS ns) = f ns

Finally, we actually attempt to run this program.
First parse the file.
If it doesn't parse, then print out the message and be done.
If it parses fine, then create a tour, find the distance, and print it.

> main :: IO ()
> main = do
>     burma14 <- parseFromFile parseData "all_tsp/burma14.tsp"
>     either print (cataNode format') burma14
>     tsps <- glob "all_tsp/*.tsp"
>     for_ (take 30 $ sort tsps) $ \tsp -> do
>         printf "Tour of %s\n" $ takeFileName tsp
>         ns <- parseFromFile parseData tsp
>         -- We just silently ignore parse errors.
>         either (const $ pure ()) (cataNode format') ns

We can see this in action by running on the terminal:
