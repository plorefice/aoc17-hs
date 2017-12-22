module Main where

import Task10

main :: IO ()
main = do
    input <- getContents
    taskA input
    taskB input

taskA :: String -> IO ()
taskA input = do
    let key  = read $ '[' : (input ++ "]") :: [Int]
        hash = knotRound key
    putStrLn . ("10a: " ++) . show $ (hash !! 0) * (hash !! 1)

taskB :: String -> IO ()
taskB input = do
    let key  = keyFromString input
        hash = knotHash key
    putStrLn . ("10b: " ++) . toHex $ hash
