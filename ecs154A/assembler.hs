import Control.Applicative hiding (many, (<|>))
import Text.ParserCombinators.Parsec

asmFile :: GenParser Char st [[String]]
asmFile = do
    result <- many line
    eof
    return result

line :: GenParser Char st [String]
line = do
    result <- instruction
    eol
    return result

eol :: GenParser Char st Char
eol = char '\n'

instruction :: GenParser Char st [String]
instruction = do
    mnem <- mnemonic
    opers <- operators
    return (mnem:opers)

mnemonic :: GenParser Char st String
mnemonic = many (noneOf " \n")

operators :: GenParser Char st [String]
operators = do
    op <- operator
    rest <- otherOps
    return (op:rest)

operator :: GenParser Char st String
operator = many (noneOf ",\n")

otherOps :: GenParser Char st [String]
otherOps =
    (char ',' >> operators)
    <|> return []

parseAsm :: String -> Either ParseError [[String]]
parseAsm = parse asmFile "wat?"
