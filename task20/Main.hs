module Main where

import Task20

main :: IO ()
main = do
  input <- getContents
  let particles = map parseParticle . lines $ input
  print particles
