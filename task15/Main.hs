module Main where

import Task15

import Data.Bits

genA :: Generator
genA = mkGen 16807 4 512

genB :: Generator
genB = mkGen 48271 8 191

main :: IO ()
main = do
    putStrLn . ("15a: " ++) . show $ taskA
    putStrLn . ("15b: " ++) . show $ taskB

taskA :: Int
taskA = length . filter matching . take 40000000 $ zip (nexts genA) (nexts genB)

taskB :: Int
taskB = length . filter matching . take 5000000 $ zip (nexts' genA) (nexts' genB)

matching :: (Int, Int) -> Bool
matching (a,b) = (a .&. 0xFFFF) == (b .&. 0xFFFF)
