module Main where

import Task18

main :: IO ()
main = do
    input <- getContents
    let instrs = map parseInstr . lines $ input
    print instrs
