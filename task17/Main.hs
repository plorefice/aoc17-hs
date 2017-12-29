module Main where

import qualified Data.Sequence as S

main :: IO ()
main = do
    input <- getContents
    taskA . read $ input
    taskB . read $ input

taskA :: Int -> IO ()
taskA ss = putStrLn . ("17a: " ++) . show . (S.!?) buffer $ idx + 1
    where buffer     = insertUntil 2018 ss
          (Just idx) = S.findIndexL (== 2017) buffer

taskB :: Int -> IO ()
taskB ss = putStrLn . ("17b: " ++) . show . (S.!?) buffer $ idx + 1
    where buffer     = insertUntil 50000000 ss
          (Just idx) = S.findIndexL (== 0) buffer

insertUntil :: Int -> Int -> S.Seq Int
insertUntil limit ss = go limit ss 0 1 (S.singleton 0)
    where
        go limit ss pos next xs
            | next == limit = xs
            | otherwise     = go limit ss pos' (next + 1) xs'
            where pos' = 1 + ((pos + ss) `mod` (S.length xs))
                  xs'  = S.insertAt pos' next xs
