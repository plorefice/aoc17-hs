module Main where

import Task18

import qualified Data.Map as M

type RegMap = M.Map Register Int

main :: IO ()
main = do
    input <- getContents
    let rm = M.fromList $ zip ('-':['a'..'z']) (cycle [0])
        il = map parseInstr . lines $ input
    putStrLn . ("18a: " ++) . show $ run rm il

run :: RegMap -> [Instr] -> Int
run rm il = go rm il 0 where
    go rm il ip = case il !! ip of
        Rcv s -> if rm M.! s /= 0 then rm M.! '-' else go rm il (ip + 1)
        instr -> go rm' il (ip + off) where (rm', off) = exec rm instr

exec :: RegMap -> Instr -> (RegMap, Int)
exec rm (Snd s)   = (upd rm '-' $ rm M.! s, 1)
exec rm (Set d e) = (upd rm d . eval rm $ e, 1)
exec rm (Add d e) = (upd rm d . (+) (rm M.! d) . eval rm $ e, 1)
exec rm (Mul d e) = (upd rm d . (*) (rm M.! d) . eval rm $ e, 1)
exec rm (Mod d e) = (upd rm d . mod (rm M.! d) . eval rm $ e, 1)
exec rm (Jgz s e) = (rm, if rm M.! s > 0 then eval rm e else 1)

upd :: RegMap -> Register -> Int -> RegMap
upd rm d v = M.update (\_ -> Just v) d rm

eval :: RegMap -> Expr -> Int
eval rm (Reg s) = rm M.! s
eval _  (Val v) = v
