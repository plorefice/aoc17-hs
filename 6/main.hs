import Data.List
import Control.Applicative

type Block = Int
type Bank  = [Block]

main = do
    input <- getContents
    let bank = map read . words $ input :: Bank
    print $ detectLoops bank []

spread :: Bank -> Bank
spread bs =
    let len      = length bs
        highest  = maximum bs
        Just idx = findIndex (== highest) bs
        removed  = map (\(i,e) -> if i == idx then 0 else e) $ zip [0..] bs
        chunks   = chunkify len . zeroPad len $ (replicate (idx+1) 0) ++ (replicate highest 1)
        toSum    = foldl1 (\xs ys -> getZipList $ (+) <$> ZipList xs <*> ZipList ys) chunks
    in  getZipList $ (+) <$> ZipList removed <*> ZipList toSum

chunkify :: Int -> [a] -> [[a]]
chunkify _ [] = []
chunkify n xs = take n xs : chunkify n (drop n xs)

zeroPad :: Int -> [Int] -> [Int]
zeroPad n xs =
    let len     = length xs
        residue = (((len + n - 1) `div` n) * n) - len
    in  xs ++ (replicate residue 0)

detectLoops :: Bank -> [Bank] -> Int
detectLoops curr prevs =
    if curr `elem` prevs then length prevs
                         else detectLoops (spread curr) (curr : prevs)
