module Main where

import Lib

main :: IO ()
main = do
    input <- getContents
    let instrs = instrList . lines $ input
    print instrs

instrList :: [String] -> [Instr]
instrList = map parseInstr
