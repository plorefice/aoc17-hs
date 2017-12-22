module Main where

import Task11

import Data.List.Split

type Cell = (Int, Int, Int)

main :: IO ()
main = do
    input <- getContents
    let dirs = map parseDir . splitOn "," $ input
        (dest, maxDist) = foldl (flip step) ((0,0,0), 0) dirs 
    putStrLn . ("11a: " ++) . show . distance (0,0,0) $ dest
    putStrLn . ("11a: " ++) . show $ maxDist
    
step :: Direction -> (Cell, Int) -> (Cell, Int)
step dir ((x, y, z), dist) = 
    let c' = case dir of
                N  -> (x+1, y-1, z)
                S  -> (x-1, y+1, z)
                NW -> (x+1, y, z-1)
                NE -> (x, y-1, z+1)
                SW -> (x, y+1, z-1)
                SE -> (x-1, y, z+1)
        dist' = distance (0,0,0) c'
    in  (c', max dist dist')

distance :: Cell -> Cell -> Int
distance (x,y,z) (x',y',z') = (abs (x - x') + abs (y - y') + abs (z - z')) `div` 2
