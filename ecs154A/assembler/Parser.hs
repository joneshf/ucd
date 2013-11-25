module Parser where

import qualified Control.Applicative as A
import Text.ParserCombinators.Parsec

asmFile :: GenParser Char st [[String]]
asmFile = many line A.<* eof

line :: GenParser Char st [String]
line = instruction A.<* eol

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
    many (char ' ') >>
    many (char '(') >>
    many (noneOf "+,)\n") A.<*
    many (char ')')

parseAsm :: String -> Either ParseError [[String]]
parseAsm = parse asmFile "Malformed input"
