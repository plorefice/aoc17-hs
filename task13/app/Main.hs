module Main where

import qualified Data.Map as M
import qualified Data.List.Split as S

type Level = (Int, Int)
type Firewall = M.Map Int Int
data Dir = Down | Up deriving (Show, Eq)
type State = (Int, M.Map Int (Dir, Int))

main :: IO ()
main = do
    input <- getContents
    let fw      = M.fromList . map parseLevel . lines $ input
        lastLvl = fst . M.findMax $ fw
        initial = (-1, M.map (\_ -> (Down, 0)) fw)
        trip'   = trip fw initial lastLvl
    putStrLn . ("13a: " ++) . show . sum . map fst $ trip'

parseLevel :: String -> Level
parseLevel s = (read $ toks !! 0, read $ toks !! 1)
    where toks = S.splitOn ": " s

advance :: Firewall -> State -> State
advance fw (pos, st) = (pos + 1, M.mapWithKey f st)
    where f lvl cur@(dir, scanner) = advance' (fw M.! lvl) cur

advance' :: Int -> (Dir, Int) -> (Dir, Int)
advance' max (dir, cur)
    | dir == Down = (if cur == (max - 2) then Up else Down, cur + 1)
    | dir == Up   = (if cur == 1 then Down else Up, cur - 1)

severity :: Firewall -> State -> Int
severity fw (pos, st) = case fw M.!? pos of
    Nothing    -> 0
    Just range -> if (snd . (M.!) st $ pos) /= 0 then 0 else pos * range

step :: Firewall -> State -> (Int, State)
step fw s@(pos, st) = (sev, next)
    where sev  = severity fw (pos + 1, st)
          next = advance fw s

trip :: Firewall -> State -> Int -> [(Int, State)]
trip _ _ (-1) = []
trip fw st rem = next : trip fw st' (rem - 1)
    where next@(_, st') = (step fw st)
