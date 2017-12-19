module Main where

import Lib

data State = Group | Garbage | Escape deriving (Show, Eq)

main :: IO ()
main = do
    input <- getContents
    let (score, garbage) = parse input
    putStrLn . ("9a: " ++) . show $ score
    putStrLn . ("9b: " ++) . show $ garbage

parse :: String -> (Int, Int)
parse s = let (_, total, garb, _) = parse' s (0, 0, 0, Group) in (total, garb)

parse' :: String -> (Int, Int, Int, State) -> (Int, Int, Int, State)
parse' "" st = st
parse' (ch:s) st = parse' s (process ch st)

process :: Char -> (Int, Int, Int, State) -> (Int, Int, Int, State)
process ch s@(cur, total, garb, st)
    | st == Group   && ch == ',' = s
    | st == Group   && ch == '{' = (cur + 1, total + (cur + 1), garb, Group)
    | st == Group   && ch == '}' = (cur - 1, total, garb, Group)
    | st == Group   && ch == '<' = (cur, total, garb, Garbage)
    | st == Group                = error $ show "invalid char in Group"
    | st == Garbage && ch == '>' = (cur, total, garb, Group)
    | st == Garbage && ch == '!' = (cur, total, garb, Escape)
    | st == Garbage              = (cur, total, garb + 1, Garbage)
    | st == Escape               = (cur, total, garb, Garbage)
    | otherwise                  = error $ show "unexpected state"