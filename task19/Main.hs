module Main where

import Data.Char
import Data.Maybe
import qualified Data.Matrix as M

type Marker    = Char
type Coords    = (Int, Int)
type Path      = M.Matrix Marker
data Direction = N | S | E | W deriving (Show, Eq)

main :: IO ()
main = do
  input <- getContents
  let path = parsePath input
      (steps, phrase) = follow path S . findEntry $ path
  putStrLn $ ("19a: " ++ phrase)
  putStrLn $ ("19b: " ++ show steps)

parsePath :: String -> Path
parsePath s = M.fromLists . map (padTo maxl) . lines $ s
  where padTo n xs = take n $ xs ++ (cycle " ")
        maxl = maximum . map length . lines $ s

findEntry :: Path -> Coords
findEntry p = (0, f p 0)
  where f :: Path -> Int -> Int
        f p y = if (p M.! (0,y)) == '|' then y else f p (y+1)

follow :: Path -> Direction -> Coords -> (Int, String)
follow p d c = go d c (1, "") where
  go d c (s,xs) =
    case step p d c of
      Just (d', c') -> go d' c' (s+1, if isLetter $ p M.! c' then xs ++ [p M.! c'] else xs)
      Nothing       -> (s,xs)

step :: Path -> Direction -> Coords -> Maybe (Direction, Coords)
step p d c =
  case advance p d c of
    Just c  -> Just (d,c)
    Nothing -> case paths of ((d', Just c'):_) -> Just (d', c')
                             []                -> Nothing
               where paths = filter (\(d', c') -> isJust c')
                           . map (\d' -> (d', advance p d' c))
                           . turns $ d

advance :: Path -> Direction -> Coords -> Maybe Coords
advance p d (x,y) = if oob c' || p M.! c' == ' ' then Nothing else Just c'
  where c' = case d of N -> (x-1, y)
                       S -> (x+1, y)
                       E -> (x, y+1)
                       W -> (x, y-1)
        oob (x,y) = if x < 0 || x >= M.rows p ||
                       y < 0 || y >= M.cols p then True else False

turns :: Direction -> [Direction]
turns N = [E,W]
turns S = [E,W]
turns E = [N,S]
turns W = [N,S]
