module Main where

type Row = [Int]
type Spreadsheet = [Row]

main = do
    input <- getContents
    let sheet = parseSheet input
    putStrLn . ("2a: " ++) . show . checksumA $ sheet
    putStrLn . ("2b: " ++) . show . checksumB $ sheet

parseSheet :: String -> Spreadsheet
parseSheet xs =
    let rows   = map words . lines $ xs
        toNums = (\r -> map read $ r :: Row)
    in  map toNums rows

checksumA :: Spreadsheet -> Int
checksumA s =
    let chk r = (maximum r) - (minimum r)
    in  sum . map chk $ s

checksumB :: Spreadsheet -> Int
checksumB s =
    let chk r = head [ x `div` y | x <- r, y <- r, x /= y && x `mod` y == 0 ]
    in  sum . map chk $ s
