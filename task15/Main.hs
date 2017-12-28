module Main where

import Task15

import Data.Bits

genA :: Generator
genA = mkGen 16807 512

genB :: Generator
genB = mkGen 48271 191

main :: IO ()
main = print . length . filter matching . take 40000000 $ zip (nexts genA) (nexts genB)
    where matching (a,b) = (a .&. 0xFFFF) == (b .&. 0xFFFF)
