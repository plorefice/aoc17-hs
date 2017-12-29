module Main where

import Task16
import Data.Char
import Data.List.Split
import Control.Monad.Primitive
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Generic.Mutable as GM

type Procs = MV.MVector RealWorld Char

main :: IO ()
main = do
    input <- getContents
    ps    <- V.thaw . V.fromList $ ['a'..'p']
    ps'   <- MV.clone ps
    let moves = map parseMove . splitOn "," $ input
    taskA ps' moves
    taskB ps moves

taskA :: Procs -> [Move] -> IO ()
taskA ps moves = do
    ps' <- routine ps moves >> V.freeze ps
    putStrLn . ("16a: " ++) . show $ ps'

taskB :: Procs -> [Move] -> IO ()
taskB ps moves = do
    ps' <- MV.clone ps
    n   <- cycles ps' moves
    ps' <- mapM_ (\_ -> routine ps moves) [1..(1000000000 `mod` n)] >> V.freeze ps
    putStrLn . ("16b: " ++) . show $ ps'

routine :: Procs -> [Move] -> IO ()
routine ps moves = mapM_ (perform ps) moves

perform :: Procs -> Move -> IO ()
perform ps (Spin n) = spin ps n
perform ps (Exchange a b) = MV.swap ps a b
perform ps (Partner a b) = partner ps a b

spin :: Procs -> Int -> IO ()
spin ps n = do
    let (a,b) = MV.splitAt (MV.length ps - n) ps
    a' <- GM.growFront a n
    mapM_ (\n' -> do
                  x <- MV.read b n'
                  MV.write a' n' x
          ) [0,1..n-1]
    MV.copy ps a'

partner :: Procs -> Char -> Char -> IO ()
partner ps a b = do
    ps' <- V.freeze ps
    let (Just a') = V.findIndex (== a) ps'
        (Just b') = V.findIndex (== b) ps'
    MV.swap ps a' b'

cycles :: Procs -> [Move] -> IO (Int)
cycles ps moves = do
    tgt <- V.freeze ps
    go 1 tgt
    where go n tgt = do
            ps' <- routine ps moves >> V.freeze ps
            if ps' == tgt then return n else go (n+1) tgt
