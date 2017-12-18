{-# LANGUAGE LambdaCase #-}
module Main where

import Lib

import qualified Data.Map as M

type Registers = M.Map Register Value

main :: IO ()
main = do
    input <- getContents
    let instrs = instrList . lines $ input
    putStrLn . ("8a: " ++) . show . maximum . M.elems . run $ instrs

instrList :: [String] -> [Instr]
instrList = map parseInstr

run :: [Instr] -> Registers
run instrs = run' instrs M.empty

run' :: [Instr] -> Registers -> Registers
run' [] regs = regs
run' il'@((Instr op@(ABinary aop rs delta) (BBinary bop rc tgt)):il) regs =
    case regs M.!? rc of
        Nothing    -> run' il' $ M.insert rc 0 regs
        (Just val) -> if cmp bop val tgt then run' il (solve op regs)
                                         else run' il regs

solve :: AExpr -> Registers -> Registers
solve (ABinary aop rs delta) regs = M.alter f rs regs
    where f = \case Just n  -> Just $ (arith aop) n delta
                    Nothing -> Just $ (arith aop) 0 delta

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
