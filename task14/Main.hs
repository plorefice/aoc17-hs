module Main where

import Task10

import Data.Bits

main :: IO ()
main = do
    input <- getContents
    let keys = map (keyFromString . (input ++) . ('-' :) . show) $ [0..127]
        ones = sum . map popCount . concat . map knotHash $ keys
    putStrLn . ("14a: " ++) . show $ ones
