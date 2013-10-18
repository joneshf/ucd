module GrayCode where

gray :: Int -> [String]
gray 0 = [""]
gray n = map ('0':) old ++ (map ('1':) . reverse) old
  where
    old = gray (n-1)

binary :: Int -> [String]
binary 0 = [""]
binary n = map ('0':) old ++ map ('1':) old
  where
    old = binary (n-1)
