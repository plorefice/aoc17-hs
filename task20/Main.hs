module Main where

import Task20

import Data.List
import Data.Function

main :: IO ()
main = do
  input <- getContents
  let particles = map parseParticle . lines $ input
  putStrLn . ("20a: " ++) . show . findClosest $ particles

{- The particle that stays closest in the long run is just the one
   with the lowest overall acceleration -}
findClosest :: [Particle] -> Int
findClosest = fst
            . head
            . sortBy (compare `on` snd)
            . zip [0..]
            . map (\(x,y,z) -> abs x + abs y + abs z)
            . map acc