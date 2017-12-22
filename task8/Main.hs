{-# LANGUAGE LambdaCase #-}
module Main where

import Task8 as Lib

import qualified Data.Map as M

type Registers = M.Map Register Value

main :: IO ()
main = do
    input <- getContents
    let (regs, max) = run . instrList . lines $ input
    putStrLn . ("8a: " ++) . show . maximum . M.elems $ regs
    putStrLn . ("8b: " ++) . show $ max

instrList :: [String] -> [Instr]
instrList = map parseInstr

run :: [Instr] -> (Registers, Int)
run instrs = run' instrs (M.empty, 0)

run' :: [Instr] -> (Registers, Int) -> (Registers, Int)
run' [] regs = regs
run' il'@((Instr op@(ABinary aop rs delta) (BBinary bop rc tgt)):il) (regs, max) =
    case regs M.!? rc of
        Nothing    -> run' il' $ (M.insert rc 0 regs, max)
        (Just val) -> if cmp bop val tgt then run' il (step op (regs, max))
                                         else run' il (regs, max)

step :: AExpr -> (Registers, Int) -> (Registers, Int)
step (ABinary aop rs delta) (regs, oldMax) =
    let f = \case Just n  -> Just $ (arith aop) n delta
                  Nothing -> Just $ (arith aop) 0 delta
        regs' = M.alter f rs regs
    in  (regs', max oldMax $ regs' M.! rs)

arith :: ArithOp -> (Int -> Int -> Int)
arith Lib.Inc = (+)
arith Lib.Dec = (-)

cmp :: BoolOp -> Int -> Int -> Bool
cmp Lib.EQ = (==)
cmp Lib.NE = (/=)
cmp Lib.LT = (<)
cmp Lib.GT = (>)
cmp Lib.LE = (<=)
cmp Lib.GE = (>=)
