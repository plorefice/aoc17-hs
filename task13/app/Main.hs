module Main where

import Data.Monoid
import qualified Data.Map as M
import qualified Data.List.Split as S

type Position  = Int
type Severity  = Int
type Level     = (Int, Int)
type Firewall  = M.Map Int Int
data Direction = Down | Up deriving (Show, Eq)
type Detection = (Bool, Severity)
type Scanners  = M.Map Int (Direction, Int)
type State     = (Position, Detection, Scanners)

main :: IO ()
main = do
    input <- getContents
    let fw      = M.fromList . map parseLevel . lines $ input
        initial = (-1, undetected, M.map (\_ -> (Down, 0)) fw)
        steps   = trip fw initial
    putStrLn . ("13a: " ++) . show . sum . map (\(_, (_, sev), _) -> sev) $ steps
    putStrLn . ("13b: " ++) . show . findDelay fw $ initial

parseLevel :: String -> Level
parseLevel s = (read $ toks !! 0, read $ toks !! 1)
    where toks = S.splitOn ": " s

undetected :: Detection
undetected = (False, 0)

detect :: Firewall -> State -> Detection
detect fw (pos, _, scanners) = case fw M.!? pos of
    Nothing    -> undetected
    Just range -> if (snd . (M.!) scanners $ pos) /= 0 then (False, 0) else (True, pos * range)

step :: Firewall -> State -> State
step fw st@(pos, _, scanners) = (pos', detection, scanners')
    where pos'      = pos + 1
          detection = detect fw (pos', undetected, scanners)
          scanners' = M.mapWithKey (\lvl s -> advance (fw M.! lvl) s) scanners

advance :: Int -> (Direction, Int) -> (Direction, Int)
advance max (dir, cur)
    | dir == Down = (if cur == (max - 2) then Up else Down, cur + 1)
    | dir == Up   = (if cur == 1 then Down else Up, cur - 1)

trip :: Firewall -> State -> [State]
trip fw st = trip' fw st (fst . M.findMax $ fw)

trip' :: Firewall -> State -> Int -> [State]
trip' _ _ (-1) = []
trip' fw st rem = next : trip' fw next (rem - 1)
    where next = step fw st

findDelay :: Firewall -> State -> Int
findDelay fw st = findDelay' fw st 0

findDelay' :: Firewall -> State -> Int -> Int
findDelay' fw st delta =
    let trip' = trip fw st
        detected = getAny . foldMap (\(_, (det, _), _) -> Any $ det) $ trip'
        (_, _, scanners') = head trip'
    in  if not detected then delta else findDelay' fw (-1, undetected, scanners') (delta + 1)
