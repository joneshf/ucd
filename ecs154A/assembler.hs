module Assembler where

import qualified Control.Applicative as A
import qualified Data.Map as M
import Text.ParserCombinators.Parsec

-- Parser stuff.

asmFile :: GenParser Char st [[String]]
asmFile = do
    ls <- many line
    eof
    return ls

line :: GenParser Char st [String]
line = do
    inst <- instruction
    eol
    return inst

eol :: GenParser Char st Char
eol = char '\n'

instruction :: GenParser Char st [String]
instruction = do
    mnem <- mnemonic
    ops <- operators
    return (mnem:ops)

mnemonic :: GenParser Char st String
mnemonic = many (noneOf " \n")

operators :: GenParser Char st [String]
operators = operator `sepBy` oneOf "+,"

operator :: GenParser Char st String
operator =
    many (char ' ') >> many (char '(') >> many (noneOf "+,)\n") A.<* many (char ')')

parseAsm :: String -> Either ParseError [[String]]
parseAsm = parse asmFile "wat?"

-- Conversion into instructions
registers :: M.Map String String
registers = M.fromList
    [ ("R0", "000")
    , ("R1", "001")
    , ("R2", "010")
    , ("R3", "011")
    , ("R4", "100")
    , ("R5", "101")
    , ("R6", "110")
    , ("R7", "111")
    ]

flags :: M.Map String String
flags = M.fromList
    [ ("Z", "000")
    , ("O", "001")
    , ("N", "010")
    , ("C", "011")
    , ("I", "100")
    , ("A", "101")
    ]

instructions :: M.Map String String
instructions = M.fromList
    [ ("ADD",  "00000")
    , ("SUB",  "00001")
    , ("ADDI", "00010")
    , ("AND",  "00011")
    , ("OR",   "00100")
    , ("XOR",  "00101")
    , ("NOT",  "00110")
    , ("INV",  "00111")
    , ("ROR",  "01000")
    , ("ROL",  "01001")
    , ("SHR",  "01010")
    , ("SHL",  "01011")
    , ("LIL",  "10000")
    , ("LIH",  "10001")
    , ("LD",   "10010")
    , ("ST",   "10011")
    , ("MOV",  "10100")
    , ("BR",   "10101")
    , ("JMP",  "10110")
    , ("JSR",  "10111")
    , ("LDFI", "11001")
    , ("MOVF", "11010")
    , ("NOP",  "11111")
    ]

findReg :: String -> String
findReg = flip (M.findWithDefault $ replicate 3 '0') registers

findInstr :: String -> String
findInstr = flip (M.findWithDefault $ replicate 5 '0') instructions

findFlag :: String -> String
findFlag = flip (M.findWithDefault $ replicate 3 '0') flags

intStrToBinStr :: String -> [Int]
intStrToBinStr = reverse . convert . read
    where
        convert 0 = [0]
        convert 1 = [1]
        convert n = snd (n `divMod` 2) : convert (fst (n `divMod` 2))

intListToStr :: [Int] -> String
intListToStr = foldr (\n acc -> show n ++ acc) ""

decToBin :: String -> String
decToBin = intListToStr . intStrToBinStr

padRight :: Int -> String -> String
padRight n s = take n $ s ++ repeat '0'

padLeft :: Int -> String -> String
padLeft n s = reverse $ take n $ reverse s ++ repeat '0'

buildInstructions :: [[String]] -> [String]
buildInstructions = map (padRight 16 . buildInstruction)

buildInstruction :: [String] -> String
buildInstruction ["BR", flag, num]           = findInstr "BR" ++ findFlag flag ++ padLeft 8 (decToBin num)
buildInstruction ["NOP"]                     = findInstr "NOP"
buildInstruction [inst, reg, 'R':rest]       = findInstr inst ++ findReg reg ++ findReg ('R':rest)
buildInstruction [inst, reg, 'R':r1, 'R':r2] = findInstr inst ++ findReg reg ++ findReg ('R':r1) ++ findReg ('R':r2)
buildInstruction [inst, reg, num]            = findInstr inst ++ findReg reg ++ padLeft 8 (decToBin num)
buildInstruction [inst, reg, r1, num]        = findInstr inst ++ findReg reg ++ findReg r1 ++ padLeft 5 (decToBin num)
buildInstruction [inst, reg]                 = findInstr inst ++ findReg reg
