module Mappings where

import qualified Data.Map as M

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

mnemonics :: M.Map String String
mnemonics = M.fromList
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

findMnem :: String -> String
findMnem = flip (M.findWithDefault $ replicate 5 '0') mnemonics

findFlag :: String -> String
findFlag = flip (M.findWithDefault $ replicate 3 '0') flags
