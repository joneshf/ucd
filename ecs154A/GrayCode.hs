module GrayCode where

--gray :: Int -> [[]]
gray 0 = []
gray 1 = ["0", "1"]
gray n = map ("0"++) old ++ map ("1"++) new
  where
    old = gray (n-1)
    new = map (\b -> if b == "0" then "1" else b) old
