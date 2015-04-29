Since we seem to be parsing the whole file eventually,
let's start encoding the grammar.
We might not use it all at the moment,
but better to have it done now.

> module P2.TSPLIB (TSPLIBKeywords(..)) where

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
