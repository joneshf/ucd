import qualified Data.Map as M
import Data.Maybe
import Text.ParserCombinators.Parsec

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
operators = operator `sepBy` char ','

operator :: GenParser Char st String
operator = char ' ' >> many (noneOf ",\n")

parseAsm :: String -> Either ParseError [[String]]
parseAsm = parse asmFile "wat?"

-- Conversion into instructions
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

lookupReg = flip M.lookup registers
lookupInstr = flip M.lookup instructions

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

buildInstructions :: [[String]] -> [String]
buildInstructions = map buildInstruction

buildInstruction :: [String] -> String
buildInstruction ["ST", reg, other] = "Fix ST"
buildInstruction ["LD", reg, other] = "Fix LD"
buildInstruction ["BR", reg, other] = "Fix BR"
buildInstruction ["NOP"]            = fromJust (lookupInstr "NOP")
buildInstruction [inst, reg, 'R':rest] = fromJust (lookupInstr inst) ++ fromJust (lookupReg reg) ++ fromJust (lookupReg ('R':rest))
buildInstruction [inst, reg, 'R':r1, 'R':r2] = fromJust (lookupInstr inst) ++ fromJust (lookupReg reg) ++ fromJust (lookupReg ('R':r1)) ++ fromJust (lookupReg ('R':r2))
buildInstruction [inst, reg, num] = fromJust (lookupInstr inst) ++ fromJust (lookupReg reg) ++ decToBin num
buildInstruction [inst, reg, r1, num] = fromJust (lookupInstr inst) ++ fromJust (lookupReg reg) ++ fromJust (lookupReg r1) ++ decToBin num
buildInstruction [inst, reg] = fromJust (lookupInstr inst) ++ fromJust (lookupReg reg)
