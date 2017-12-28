module Task15
( Generator
, mkGen
, next
, nexts
) where

data Generator = Generator Int Int

mkGen :: Int -> Int -> Generator
mkGen f n = Generator f n

next :: Generator -> (Int, Generator)
next (Generator f n) = (k, Generator f k)
    where k = (n * f) `mod` 2147483647

nexts :: Generator -> [Int]
nexts g = n' : nexts g'
    where (n', g') = next g
