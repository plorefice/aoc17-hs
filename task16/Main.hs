module Main where

import Task16
import Data.List.Split
import Control.Monad.Primitive
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Generic.Mutable as GM

type Procs = MV.MVector RealWorld Char

main :: IO ()
main = do
    input  <- getContents
    procs  <- lineup
    let moves = map parseMove . splitOn "," $ input
    procs' <- mapM_ (perform procs) moves >> V.freeze procs
    putStrLn . ("16a: " ++) . show $ procs'

lineup :: IO (Procs)
lineup = V.thaw . V.fromList $ ['a'..'p']

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
