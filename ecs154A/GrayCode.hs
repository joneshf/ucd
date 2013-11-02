module GrayCode where

generateBits :: ([String] -> [String]) -> Int -> [String]
generateBits _ 0 = [""]
generateBits f n = map ('0':) old ++ (map ('1':) . f) old
  where
    old = generateBits f (n-1)

gray :: Int -> [String]
gray = generateBits reverse

binary :: Int -> [String]
binary = generateBits id
