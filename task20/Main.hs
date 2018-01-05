module Main where

import Task20

import Data.List
import Data.Function

main :: IO ()
main = do
  input <- getContents
  let particles = map parseParticle . lines $ input
  putStrLn . ("20a: " ++) . show . findClosest $ particles
  {- There's probably a valid stop condition -}
  putStrLn . ("20b: " ++) . show . length . foldr ($) particles $ [clip . step | _ <- [1..500]]

{- The particle that stays closest in the long run is just the one
   with the lowest overall acceleration -}
findClosest :: [Particle] -> Int
findClosest = fst
            . head
            . sortBy (compare `on` snd)
            . zip [0..]
            . map (\(x,y,z) -> abs x + abs y + abs z)
            . map acc

step :: [Particle] -> [Particle]
step [] = []
step (p:ps) = update p : step ps
  where update p = let p' = p  { vel = vel p .+. acc p }
                   in       p' { pos = pos p .+. vel p' }

clip :: [Particle] -> [Particle]
clip ps = map (ps !!)
        . map fst
        . concat
        . filter ((== 1) . length)
        . groupBy ((==) `on` snd)
        . sortBy (compare `on` snd)
        . zip [0..]
        . map pos
        $ ps

(.+.) :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
(x,y,z) .+. (x',y',z') = (x+x', y+y', z+z')