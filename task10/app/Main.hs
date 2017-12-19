module Main where

import Data.Bits
import Data.Char
import Data.List.Split
import Numeric

type Pos = Int
type Skip = Int
type Hash  = [Int]
type State = (Pos, Skip)

main :: IO ()
main = do
    input <- getContents
    taskA input
    taskB input

taskA :: String -> IO ()
taskA input = do
    let lengths    = read $ '[' : (input ++ "]") :: [Int]
        (hash', _) = foldl (\(hs, st) len -> hash hs st len) ([0..255], (0,0)) lengths
    putStrLn . ("10a: " ++) . show $ (hash' !! 0) * (hash' !! 1)

taskB :: String -> IO ()
taskB input = do
    let lengths     = (map ord input) ++ [17, 31, 73, 47, 23]
        round h     = foldl (\(hs', st') len -> hash hs' st' len) h lengths
        (sparse, _) = foldl (\acc rn -> round acc) ([0..255], (0,0)) [0..63]
    putStrLn . ("10b: " ++) . show . hexfmt . dense $ sparse

hash :: Hash -> State -> Int -> (Hash, State)
hash hs st@(pos, skip) len = (hs', (pos', skip'))
    where hs'   = hash' hs pos len
          pos'  = (pos + len + skip) `mod` length hs
          skip' = skip + 1

hash' :: Hash -> Int -> Int -> Hash
hash' hs pos n =
    let extended = concat . replicate 2 $ hs
        hashed   = reverse . take n . drop pos $ extended
        surplus  = max ((pos + length hashed) - length hs) 0
        actual   = length hashed - surplus
    in  if surplus == 0
        then (take pos hs) ++ hashed ++ (drop (pos + actual) hs)
        else let hlen = length hashed - surplus
                 mlen = length hs - surplus - actual
                 tlen = actual
             in  (drop hlen hashed) ++ (take mlen . drop surplus $ hs) ++ (take tlen hashed)

dense :: Hash -> [Int]
dense = map (foldl1 xor) . chunksOf 16

hexfmt :: [Int] -> String
hexfmt = concat . map (\n -> showHex n "")
