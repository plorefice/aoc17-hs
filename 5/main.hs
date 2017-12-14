import Data.List

type IP = Int
type JumpList = [Int]
type Program = (IP,JumpList)

main = do
    jumpStr <- readFile "input.txt"
    let jumpList = map read . lines $ jumpStr :: JumpList
    {- first and last jumps aren't actually jumps -}
    putStrLn . ("5a: " ++) . show . abs . (-) 2 . length . solve updateProgA . newProgram $ jumpList
    putStrLn . ("5b: " ++) . show . abs . (-) 2 . length . solve updateProgB . newProgram $ jumpList

newProgram :: JumpList -> Program
newProgram jl = (0, jl)

updateProgA :: Program -> Program
updateProgA (ip, jl) =
    let current = jl !! ip
        next    = ip + current
        bottom  = take ip jl
        top     = reverse . take (length jl - ip - 1) . reverse $ jl
    in  (next, bottom ++ (current + 1 : top))

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

solve :: (Program -> Program) -> Program -> [IP]
solve upd prog =
    let nips = case next upd prog of
            Left  nip   -> [nip]
            Right nprog -> solve upd nprog
    in (fst prog) : nips
