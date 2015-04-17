module P1Test where

import P1 (Node(..), tour, tourDistance)

import Data.Tuple (swap)

import Test.QuickCheck

instance Arbitrary Node where
    arbitrary = Node <$> arbitrary <*> choose (-90, 90) <*> choose (-180, 180)

prop_Reflexive :: Node -> Bool
prop_Reflexive i = 0 == (tourDistance $ tour [i])

prop_NonNegative :: Node -> Node -> Bool
prop_NonNegative i j = tourDistance [(i, j)] >= 0

prop_Symmetric :: Node -> Node -> Bool
prop_Symmetric i j = tourDistance [(i, j)] == tourDistance [(j, i)]

prop_Triangle :: Node -> Node -> Node -> Bool
prop_Triangle i j k = tourDistance [(i, k)] <= tourDistance [(i, j), (j, k)]

prop_SymmetricTSP :: [Node] -> Bool
prop_SymmetricTSP ns =
    tourDistance (tour ns) == tourDistance (tour $ reverse ns)

prop_ShiftTSP :: [Node] -> Int -> Bool
prop_ShiftTSP ns n = let n' = n `mod` length ns in
    tourDistance (tour ns) == tourDistance (tour . uncurry (++) . swap . splitAt n $ ns)

main :: IO ()
main = do
    -- quickCheck prop_Reflexive
    quickCheck prop_NonNegative
    quickCheck prop_Symmetric
    quickCheck prop_Triangle
    quickCheck prop_SymmetricTSP
    quickCheck prop_ShiftTSP
