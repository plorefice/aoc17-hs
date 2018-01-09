module Main where

import Task23

import Data.Numbers.Primes
import qualified Data.Map as M

data Process
  = Process
  { ip   :: Int
  , regs :: M.Map Char Int
  , muls :: Int
  } deriving (Show)

main :: IO ()
main = do
  input <- getContents
  let program = map parseInstr . lines $ input
  putStrLn . ("23a: " ++) . show . muls . run program $ proc
  putStrLn . ("23b: " ++) . show . length . filter (not . isPrime) $ [109300,109317..126300]

proc :: Process
proc = Process { ip   = 0
               , regs = M.fromList $ zip ['a'..'h'] (cycle [0])
               , muls = 0
               }

run :: [Instr] -> Process -> Process
run il p
   | ip p < 0 || ip p >= length il = p
   | otherwise = run il (exec (il !! ip p) p)

exec :: Instr -> Process -> Process
exec (Set d e) p =        step . setReg p d . id            . eval p $ e
exec (Sub d e) p =        step . setReg p d . (-) (reg p d) . eval p $ e
exec (Mul d e) p = tmul . step . setReg p d . (*) (reg p d) . eval p $ e
exec (Jnz c e) p = p { ip = if eval p c /= 0 then ip p + (eval p e) else ip p + 1 }

step :: Process -> Process
step p = p { ip = ip p + 1 }

setReg :: Process -> Char -> Int -> Process
setReg p r v = p { regs = M.insert r v (regs p) }

reg :: Process -> Register -> Int
reg p r = (regs p) M.! r

eval :: Process -> Expr -> Int
eval p (Reg r) = (regs p) M.! r
eval p (Val e) = e

tmul :: Process -> Process
tmul p = p { muls = muls p + 1 }
