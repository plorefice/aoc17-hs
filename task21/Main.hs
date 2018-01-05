{-# LANGUAGE PackageImports #-}
module Main where

import Debug.Trace
import Data.List
import Data.List.Split (splitOn, chunksOf)

type Pattern = [[Char]]
type Rule    = (Pattern, Pattern)

main :: IO ()
main = do
  input <- getContents
  let rules  = map toRule . lines $ input
      go n   = iterate (step rules) [".#.","..#","###"] !! n
      pixels = sum . map (length . filter (== '#'))
  putStrLn . ("21a: " ++) . show . pixels . go $ 5
  putStrLn . ("21b: " ++) . show . pixels . go $ 18

toRule :: String -> Rule
toRule s = (a, b) where (a:b:[]) = map (splitOn "/") . splitOn " => " $ s

step :: [Rule] -> Pattern -> Pattern
step rs p = assemble n . map (map $ subst rs) . zoom n $ p
  where n = 2 + mod (length p) 2

zoom :: Int -> Pattern -> [[Pattern]]
zoom n = map (transpose . map (chunksOf n)) . chunksOf n

assemble :: Int -> [[Pattern]] -> Pattern
assemble n = concatMap (\ps -> map (flip concatMap ps . flip (!!)) [0..n])

subst :: [Rule] -> Pattern -> Pattern
subst rs p = snd r where (Just r) = find ((`elem` ts) . fst) rs
                         ts = [ t p | t <- transforms]

transforms :: [Pattern -> Pattern]
transforms = (.) <$> [ id, flipr, flipc ] <*> [ r90, r180, r270 ]
             where flipr = reverse
                   flipc = map reverse
                   r90   = flipr . transpose
                   r180  = flipc . flipr
                   r270  = flipc . transpose
