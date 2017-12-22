module Task10
( keyFromString
, knotRound
, knotHash
, toHex
) where

import Data.Char
import Data.List.Split
import Data.Bits
import Numeric

type Pos = Int
type Skip = Int
type Key = [Int]
type Hash = [Int]
type State = (Pos, Skip)

keyFromString :: String -> Key
keyFromString s = (map ord s) ++ [17, 31, 73, 47, 23]

knotRound :: Key -> Hash
knotRound key = fst . Task10.round [0..255] (0,0) $ key

knotHash :: Key -> Hash
knotHash key = dense hs'
    where (hs', _ ) = foldl (\(hs, st) _ -> Task10.round hs st key) ([0..255], (0,0)) [0..63]

round :: Hash -> State -> Key -> (Hash, State)
round hs st = foldl (\(hs, st) len -> hash hs st len) (hs, st)

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

dense :: Hash -> Hash
dense = map (foldl1 xor) . chunksOf 16

toHex :: Hash -> String
toHex = concat . map (\n -> showHex n "")
