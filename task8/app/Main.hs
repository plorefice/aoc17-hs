module Main where

import Lib

main :: IO ()
main = do
    input <- getContents
    let instrs = instrList . lines $ input
    print "WIP"

instrList :: [String] -> [Instr]
instrList = map parseInstr
