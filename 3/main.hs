data Direction = R | U | L | D deriving Eq
type Path = [Direction]
type Grid = [Int]

{- This is probably the most inefficient piece of code ever written -}

main = do
    putStrLn . ("3a: " ++) . show . manhattan origin . walk $ 277678
    putStrLn . ("3b: " ++) . show . head . dropWhile (<277678) . map (valueAt . walk) $ [1..]

origin :: Path
origin = []

spiral :: Path
spiral =
    let dirs  = concat . repeat $ [R,U,L,D]
        steps = concat . map (\x -> [x,x]) $ [1..]
    in  concat $ zipWith replicate steps dirs

walk :: Int -> Path
walk n = take (n-1) spiral

subpaths :: Path -> [Path]
subpaths [] = [[]]
subpaths p = p : subpaths (init p)

neighbours :: Path -> [Path]
neighbours p = take 4 . filter (\p' -> euclidean' p p' <= 2) . subpaths . init $ p

valueAt :: Path -> Int
valueAt [] = 1
valueAt p  = sum . map valueAt . neighbours $ p

{- Distance computation functions -}

position :: Path -> (Int,Int)
position [] = (0,0)
position (d:ds)
    | d == R = (x+1,y)
    | d == L = (x-1,y)
    | d == U = (x,y+1)
    | d == D = (x,y-1)
    where (x,y) = position ds

manhattan :: Path -> Path -> Int
manhattan p1 p2 =
    let (x1,y1) = position p1 
        (x2,y2) = position p2
    in  abs (x2-x1) + abs (y2-y1)

{- Actually squared Eucledian -}
euclidean' :: Path -> Path -> Int
euclidean' p1 p2 =
    let (x1,y1) = position p1 
        (x2,y2) = position p2
    in  (x2-x1)^2 + (y2-y1)^2
