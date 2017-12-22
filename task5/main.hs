module Main where

import Data.List

type IP = Int
type JumpList = [Int]
type Program = (IP,JumpList)

main = do
    jumpStr <- getContents
    let jumpList = map read . lines $ jumpStr :: JumpList
    {- The last jump is bogus, hence the (-) 1 -}
    putStrLn . ("5a: " ++) . show . abs . (-) 1 . solve instrUpdA . newProgram $ jumpList
    putStrLn . ("5b: " ++) . show . abs . (-) 1 . solve instrUpdB . newProgram $ jumpList

newProgram :: JumpList -> Program
newProgram jl = (0, jl)

instrUpdA :: Int -> Int
instrUpdA c = c + 1

instrUpdB :: Int -> Int
instrUpdB c = if c >= 3 then c - 1 else c + 1

updateProg :: (Int -> Int) -> Program -> Program
updateProg upd (ip, jl) =
    let current = jl !! ip
        next    = ip + current
        instr   = upd current
        bottom  = take ip jl
        top     = drop (ip + 1) jl
    in  (next, bottom ++ (instr : top))

next :: (Int -> Int) -> Program -> Either IP Program
next upd (ip, jl)
    | ip >= length jl = Left ip
    | otherwise       = Right $ updateProg upd (ip, jl)

solve :: (Int -> Int) -> Program -> Int
solve upd prog =
    let n = case next upd prog of
            Left  _     -> 0
            Right nprog -> solve upd nprog
    in  n + 1
