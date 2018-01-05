{-# LANGUAGE PackageImports #-}
module Main where

import Debug.Trace
import Data.List
import Data.List.Split (splitOn, chunksOf)
import qualified "matrix" Data.Matrix as M

type Pattern = M.Matrix Char
type Rule    = (Pattern, Pattern)

main :: IO ()
main = do
  input <- getContents
  let rules = map toRule . lines $ input
      go n  = foldr ($) (M.fromLists [".#.","..#","###"]) $ [ apply rules | _ <- [1..n] ]
  putStrLn . ("21a: " ++) . show . foldl (\n c -> if c == '#' then n + 1 else n) 0 . go $ 5

toRule :: String -> Rule
toRule s = (f pi, f po)
  where (pi:po:[]) = splitOn " => " s
        f = M.fromLists . splitOn "/"

size :: Pattern -> Int
size = M.nrows

apply :: [Rule] -> Pattern -> Pattern
apply rs p
  | size p `mod` 2 == 0 = apply' 2 $ p </> 2
  | size p `mod` 3 == 0 = apply' 3 $ p </> 3
  where apply' n = foldl1 (M.<->)
                 . map (foldl1 (M.<|>))
                 . chunksOf (size p `div` n)
                 . map (subst rs)

(</>) :: Pattern -> Int -> [Pattern]
p </> n = go (1,1) (size p)
  where go (r,c) s
          | r > s     = []
          | otherwise = M.submatrix r r' c c' p : go (r + n*(c' `div` s), (c'+1) `mod` s) s
                        where r' = r + n - 1
                              c' = c + n - 1

subst :: [Rule] -> Pattern -> Pattern
subst rs p = snd r where (Just r) = find ((`elem` ts) . fst) rs
                         ts = [ t p | t <- transforms]

transforms :: [Pattern -> Pattern]
transforms = (.) <$> [ id, flipr, flipc ] <*> [ r90, r180, r270 ]
             where flipr = M.fromLists . reverse . M.toLists
                   flipc = M.fromLists . map reverse . M.toLists
                   r90   = flipr . M.transpose
                   r180  = flipc . flipr
                   r270  = flipc . M.transpose
