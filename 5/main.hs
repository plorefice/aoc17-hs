import Data.List

type IP = Int
type JumpList = [Int]
type Program = (IP,JumpList)

main = do
    jumpStr <- readFile "input.txt"
    let jumpList = map read . lines $ jumpStr :: JumpList
    {- The last jump is bogus, hence the (-) 1 -}
    putStrLn . ("5a: " ++) . show . abs . (-) 1 . solve updateProgA . newProgram $ jumpList
    putStrLn . ("5b: " ++) . show . abs . (-) 1 . solve updateProgB . newProgram $ jumpList

newProgram :: JumpList -> Program
newProgram jl = (0, jl)

updateProgA :: Program -> Program
updateProgA (ip, jl) =
    let next    = ip + (jl !! ip)
        updated = map (\(i,e) -> if i == ip then e + 1 else e) $ zip [0..] jl
    in  (next, updated)

updateProgB :: Program -> Program
updateProgB (ip, jl) =
    let current = jl !! ip
        next    = ip + current
        instr   = if current >= 3 then current - 1 else current + 1
        bottom  = take ip jl
        top     = reverse . take (length jl - ip - 1) . reverse $ jl
    in  (next, bottom ++ (instr : top))

next :: (Program -> Program) -> Program -> Either IP Program
next upd (ip, jl)
    | ip >= length jl = Left ip
    | otherwise       = Right $ upd (ip, jl)

solve :: (Program -> Program) -> Program -> Int
solve upd prog =
    let n = case next upd prog of
            Left  _     -> 0
            Right nprog -> solve upd nprog
    in  n + 1
