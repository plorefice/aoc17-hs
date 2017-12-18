import Data.List
import Data.Char

main = do
    input <- readFile "input.txt"
    let partial = solveA $ input
        solA = if (head input) == (last input) then partial + read [(head input)] else partial
        solB = solveB $ input
    putStrLn . ("1a: " ++) . show $ solA
    putStrLn . ("1b: " ++) . show $ solB

solveA :: String -> Int
solveA captcha =
    let grouped = filter ((>1) . length) . group $ captcha
        value e = read [(head e)]
        accum   = (\total e -> total + (value e) * (length e - 1))
    in  foldl accum 0 grouped

solveB :: String -> Int
solveB captcha =
    let (l1, l2) = splitAt (length captcha `div` 2) captcha
        same     = filter (\(a,b) -> a == b) $ zip l1 l2
        digits   = map (digitToInt . fst) same
    in  sum digits * 2
