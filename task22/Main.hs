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
  (_,_,infections)  <- iterN (burst infect grid) (N, (0,0), 0) 10000
  putStrLn . ("22a: " ++) . show $ infections
  grid <- fromInput input
  (_,_,infections') <- iterN (burst infect' grid) (N, (0,0), 0) 10000000
  putStrLn . ("22b: " ++) . show $ infections'

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

burst :: (Grid -> Virus -> IO (Virus)) -> Grid -> Virus -> IO (Virus)
burst f g v@(d,p,inf) = do
  node' <- H.lookup g p
  case node' of
    Nothing   -> H.insert g p '.' >> burst f g v
    Just node -> f g (turn d node, p, inf) >>= step g

turn :: Direction -> Node -> Direction
turn d n
  | n == '#' = turn 1
  | n == '.' = turn (-1)
  | n == 'W' = turn 0
  | n == 'F' = turn 2
  where turn n = let (Just idx) = elemIndex d dirs in dirs !! ((idx + n) `mod` 4)
        dirs = [N,E,S,W]

infect :: Grid -> Virus -> IO (Virus)
infect g (d,p,inf) = do
  (Just node) <- H.lookup g p
  case node of
    '#' -> H.insert g p '.' >> return (d, p, inf)
    '.' -> H.insert g p '#' >> return (d, p, inf+1)

infect' :: Grid -> Virus -> IO (Virus)
infect' g (d,p,inf) = do
  (Just node) <- H.lookup g p
  case node of
    '#' -> H.insert g p 'F' >> return (d, p, inf)
    '.' -> H.insert g p 'W' >> return (d, p, inf)
    'W' -> H.insert g p '#' >> return (d, p, inf+1)
    'F' -> H.insert g p '.' >> return (d, p, inf)

step :: Grid -> Virus -> IO (Virus)
step _ (d, (x,y), inf) = return (d,p',inf)
  where p' = case d of
               N -> (x-1, y)
               S -> (x+1, y)
               W -> (x, y-1)
               E -> (x, y+1)
