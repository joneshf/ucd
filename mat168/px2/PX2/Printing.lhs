And of course we want some way to print this stuff out prettily.

> module PX2.Printing where

> import Text.PrettyPrint.Boxes

> optimalTours :: [String]
> optimalTours =
>     [ "7542"
>     , "3323"
>     , "55209"
>     , "6859"
>     , "7013"
>     ]

> prettyTable :: [[String]] -> String
> prettyTable = prettyCols
>
> -- The `boxes` api isn't so great to use, but quite powerful.
> prettyCols :: [[String]] -> String
> prettyCols = render . hsep 2 left . mkHeaders . map (vcat left . map text)
>     where
>     mkHeaders = zipWith mkHeaders' headers
>     mkHeaders' h c = h // separator (max (cols h) (cols c)) // c
>     separator n = text (replicate n '-')
>     headers = map text [ "Filename"
>                        , "Optimal"
>                        , "Shortcut"
>                        , "Greedy"
>                        , "Christofides"
>                        ]
