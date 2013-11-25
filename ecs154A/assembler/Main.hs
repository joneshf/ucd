module Main where

import System.Environment
import Assembler
import Parser

main :: IO ()
main = do
    [i, o] <- getArgs
    asm <- readFile i
    case parseAsm asm of
        Left e  -> print e
        Right p -> do
            let bin = (unlines . buildInstructions) p
            writeFile o bin
