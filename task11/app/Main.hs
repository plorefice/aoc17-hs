module Main where

import Data.List.Split

import Lib

type Cell = (Int, Int, Int)

main :: IO ()
main = do
    input <- getContents
    let dirs = map parseDir . splitOn "," $ input
        dest = foldl (flip step) (0,0,0) dirs 
    putStrLn . ("11a: " ++) . show . distance (0,0,0) $ dest
    
step :: Direction -> Cell -> Cell
step dir (x, y, z) = case dir of
    N  -> (x+1, y-1, z)
    S  -> (x-1, y+1, z)
    NW -> (x+1, y, z-1)
    NE -> (x, y-1, z+1)
    SW -> (x, y+1, z-1)
    SE -> (x-1, y, z+1)

distance :: Cell -> Cell -> Int
distance (x,y,z) (x',y',z') = (abs (x - x') + abs (y - y') + abs (z - z')) `div` 2
