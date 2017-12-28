module Task15
( Generator
, mkGen
, nexts
, nexts'
) where

data Generator = Generator Int Int Int

mkGen :: Int -> Int -> Int -> Generator
mkGen f m n = Generator f m n

next :: Generator -> (Int, Generator)
next (Generator f m n) = (k, Generator f m k)
    where k = (n * f) `mod` 2147483647

nexts :: Generator -> [Int]
nexts g = n' : nexts g'
    where (n', g') = next g

nexts' :: Generator -> [Int]
nexts' g@(Generator f m n)
    | n' `mod` m == 0 = n' : nexts' g'
    | otherwise       = nexts' g'
    where (n', g') = next g