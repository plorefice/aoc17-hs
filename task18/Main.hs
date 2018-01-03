module Main where

import Task18

import qualified Data.Map as M

type Registers = M.Map Register Int

data Frequency
    = Undefined
    | Freq Int
    deriving (Show, Eq)

data Program
    = Program
    { ip   :: Int
    , regs :: Registers
    , freq :: Frequency
    , rcvq :: [Int]
    , sndc :: Int
    } deriving (Show, Eq)

main :: IO ()
main = do
    input <- getContents
    let il = map parseInstr . lines $ input
    taskA il >> taskB il

{- Task A -}

taskA :: [Instr] -> IO ()
taskA il = putStrLn . ("18a: " ++) . show $ runA newProgram il

runA :: Program -> [Instr] -> Int
runA prog il =
    case freq prog of
        Undefined -> runA prog' il where prog' = execA prog (il !! (ip prog))
        Freq f    -> f

execA :: Program -> Instr -> Program
execA p (Set d e) = step . setReg p d .                 eval p $ e
execA p (Add d e) = step . setReg p d . (+) (reg p d) . eval p $ e
execA p (Mul d e) = step . setReg p d . (*) (reg p d) . eval p $ e
execA p (Mod d e) = step . setReg p d . mod (reg p d) . eval p $ e
execA p (Jgz c e) = p { ip = (+) (ip p) $ if (eval p c) > 0 then eval p e else 1 }
execA p (Snd e)   = step . setReg p '-' . eval p $ e
execA p (Rcv s)   = step . setFreq p $ if (reg p s) /= 0 then Freq (reg p '-') else Undefined

{- Task B -}

taskB :: [Instr] -> IO ()
taskB il = do
    let p0  = setReg newProgram 'p' 0
        p1  = setReg newProgram 'p' 1
    putStrLn . ("18b: " ++) . show . sndc . snd . runB (p0, p1) $ il

runB :: (Program, Program) -> [Instr] -> (Program, Program)
runB (p0, p1) il =
    case execB p0 p1 $ il !! (ip p0) of
        (Right p0, p1) -> runB (p0, p1) il
        (Left  p0, p1) -> case execB p1 p0 $ il !! (ip p1) of
                              (Right p1, p0) -> runB (p0, p1) il
                              (Left  p1, p0) -> (p0, p1)

execB :: Program -> Program -> Instr -> (Either Program Program, Program)
execB p p' (Set d e) = (Right . step . setReg p d .                 eval p $ e, p')
execB p p' (Add d e) = (Right . step . setReg p d . (+) (reg p d) . eval p $ e, p')
execB p p' (Mul d e) = (Right . step . setReg p d . (*) (reg p d) . eval p $ e, p')
execB p p' (Mod d e) = (Right . step . setReg p d . mod (reg p d) . eval p $ e, p')
execB p p' (Jgz c e) = (Right p { ip = (+) (ip p) $ if (eval p c) > 0 then eval p e else 1 }, p')
execB p p' (Snd e)   = (Right . step $ p { sndc = sndc p + 1 }, p' { rcvq = rcvq p' ++ [eval p e]})
execB p p' (Rcv s)   = (pop p s, p')
    where pop p s = case rcvq p of
                        (x:xs) -> Right . step . setReg (p { rcvq = xs }) s $ x
                        []     -> Left p

{- Common -}

newProgram :: Program
newProgram = Program { ip   = 0
                     , regs = M.fromList $ zip ('-':['a'..'z']) (cycle [0])
                     , freq = Undefined
                     , rcvq = []
                     , sndc = 0
                     }

setFreq :: Program -> Frequency -> Program
setFreq p f = p { freq = f }

setReg :: Program -> Register -> Int -> Program
setReg p d v = p { regs = M.update (\_ -> Just v) d (regs p) }

step :: Program -> Program
step p = p { ip = ip p + 1 }

reg :: Program -> Register -> Int
reg p s = (regs p) M.! s

eval :: Program -> Expr -> Int
eval p (Reg s) = reg p s
eval _ (Val v) = v
