module Main where

import Lib

data State = Group | Garbage | Escape deriving (Show, Eq)

main :: IO ()
main = do
    input <- getContents
    putStrLn . ("9a: " ++) . show . parse $ input

parse :: String -> Int
parse s = let (_, total, _) = parse' s (0, 0, Group) in total

parse' :: String -> (Int, Int, State) -> (Int, Int, State)
parse' "" st = st
parse' (ch:s) st = parse' s (process ch st)

process :: Char -> (Int, Int, State) -> (Int, Int, State)
process ch s@(cur, total, st)
    | st == Group   && ch == ',' = s
    | st == Group   && ch == '{' = (cur + 1, total + (cur + 1), Group)
    | st == Group   && ch == '}' = (cur - 1, total, Group)
    | st == Group   && ch == '<' = (cur, total, Garbage)
    | st == Group                = error $ show "invalid char in Group"
    | st == Garbage && ch == '>' = (cur, total, Group)
    | st == Garbage && ch == '!' = (cur, total, Escape)
    | st == Garbage              = s
    | st == Escape               = (cur, total, Garbage)
    | otherwise                  = error $ show "unexpected state"