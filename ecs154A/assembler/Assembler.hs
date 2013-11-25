module Assembler where

import Mappings

intStrToBinList :: String -> [Int]
intStrToBinList = reverse . convert . read
    where
        convert 0 = [0]
        convert 1 = [1]
        convert n = snd (n `divMod` 2) : convert (fst (n `divMod` 2))

intListToStr :: [Int] -> String
intListToStr = foldr (\n acc -> show n ++ acc) ""

decToBin :: String -> String
decToBin = intListToStr . intStrToBinList

padRight :: Int -> String -> String
padRight n s = take n $ s ++ repeat '0'

padLeft :: Int -> String -> String
padLeft n s = reverse $ take n $ reverse s ++ repeat '0'

buildInstructions :: [[String]] -> [String]
buildInstructions = map (padRight 16 . buildInstruction)

buildInstruction :: [String] -> String
buildInstruction ["BR", flag, num]           =
    findMnem "BR" ++ findFlag flag ++ padLeft 8 (decToBin num)
buildInstruction ["NOP"]                     =
    findMnem "NOP"
buildInstruction [inst, reg, 'R':rest]       =
    findMnem inst ++ findReg reg ++ findReg ('R':rest)
buildInstruction [inst, reg, 'R':r1, 'R':r2] =
    findMnem inst ++ findReg reg ++ findReg ('R':r1) ++ findReg ('R':r2)
buildInstruction [inst, reg, num]            =
    findMnem inst ++ findReg reg ++ padLeft 8 (decToBin num)
buildInstruction [inst, reg, r1, num]        =
    findMnem inst ++ findReg reg ++ findReg r1 ++ padLeft 5 (decToBin num)
buildInstruction [inst, reg]                 =
    findMnem inst ++ findReg reg
