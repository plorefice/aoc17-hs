module Main where

import Debug.Trace
import Data.Bits
import Data.List
import Control.Monad
import qualified Data.HashTable.IO as H

type Position  = (Int, Int)
type Node      = Char
data Direction = N | S | E | W deriving (Show, Eq)
type Virus     = (Direction, Position, Int)
type Grid      = H.BasicHashTable Position Node

main :: IO ()
main = do
  input <- getContents
  grid <- fromInput input
  (_,_,infections) <- iterN (burst grid) (N, (0,0), 0) 10000
  putStrLn . ("22a: " ++) . show $ infections

iterN :: Monad m => (a -> m a) -> a -> Int -> m a
iterN f x n = case n of
  0 -> pure x
  _ -> do
    x' <- f x
    iterN f x' (n-1)

fromInput :: String -> IO (Grid)
fromInput s = do
  let xs = lines s
  let rx = [negate . (`div` 2) . length $ xs      .. (`div` 2) . length $ xs     ]
  let ry = [negate . (`div` 2) . length $ xs !! 0 .. (`div` 2) . length $ xs !! 0]
  g <- H.new
  mapM_ (\(x,xs') -> mapM_ (\(y,v) -> H.insert g (x,y) v) $ zip ry xs') $ zip rx xs
  return g

burst :: Grid -> Virus -> IO (Virus)
burst g v@(d,p,inf) = do
  node' <- H.lookup g p
  case node' of
    Nothing   -> H.insert g p '.' >> burst g v
    Just node -> infect g (turn d node, p, inf) >>= step g

turn :: Direction -> Node -> Direction
turn d n
  | n == '#' = let (Just idx) = elemIndex d dirs in dirs !! ((idx + 1) `mod` 4)
  | n == '.' = let (Just idx) = elemIndex d dirs in dirs !! ((idx - 1) `mod` 4)
  where dirs = [N,E,S,W]

infect :: Grid -> Virus -> IO (Virus)
infect g (d,p,inf) = do
  (Just node) <- H.lookup g p
  case node of
    '#' -> H.insert g p '.' >> return (d,p,inf)
    '.' -> H.insert g p '#' >> return (d,p,inf+1)

step :: Grid -> Virus -> IO (Virus)
step _ (d, (x,y), inf) = return (d,p',inf)
  where p' = case d of
               N -> (x-1, y)
               S -> (x+1, y)
               W -> (x, y-1)
               E -> (x, y+1)
