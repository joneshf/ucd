We need to parse in the distance files.

> module PX2.Parser (parseData) where
>
> import PX2.Distance
>
> import Text.Parsec (count, spaces)
> import Text.Parsec.String (Parser)
> import Text.ParserCombinators.Parsec.Char (CharParser)
> import Text.ParserCombinators.Parsec.Number (nat)
>
> parseData :: Parser [Distance]
> parseData = do
>     m <- nat'
>     let n = m * (m - 1) `div` 2
>     count n parseDistance
>
> parseDistance :: Parser Distance
> parseDistance = Distance <$> nat' <*> nat' <*> nat'

We want to parse `nat`s with spaces around them.

> nat' :: Integral i => CharParser st i
> nat' = spaces *> nat <* spaces
