module Main where

import Task10

import Data.Bits
import Data.List
import Control.Monad
import Control.Monad.Primitive
import qualified Data.Matrix as M
import qualified Data.Vector as V
import qualified Data.Matrix.Mutable as MM
import qualified Data.Matrix.Generic.Mutable as GM

type Label  = Int
type Disk   = [Integer]
type Bitmap = MM.MMatrix RealWorld Label

{- Connected-component labeling
1. Start from the first pixel in the image. Set current label to 1. Go to (2).
2. If this pixel is a foreground pixel and it is not already labelled,
   give it the current label and add it as the first element in a queue,
   then go to (3). If it is a background pixel or it was already labelled,
   then repeat (2) for the next pixel in the image.
3. Pop out an element from the queue, and look at its neighbours
   (based on any type of connectivity). If a neighbour is a foreground pixel
   and is not already labelled, give it the current label and add it to the queue.
   Repeat (3) until there are no more elements in the queue.
4. Go to (2) for the next pixel in the image and increment current label by 1. -}

main :: IO ()
main = do
    input <- getContents
    let keys   = map (keyFromString . (input ++) . ('-' :) . show) $ [0..127]
        hashes = map (map fromIntegral . knotHash) keys
        disk   = map (foldl1 (\acc n -> acc `shiftL` 8 .|. n)) hashes :: Disk
    taskA disk
    taskB disk

taskA :: Disk -> IO ()
taskA disk = putStrLn . ("14a: " ++) . show $ ones
    where ones = sum . map popCount $ disk

taskB :: Disk -> IO ()
taskB disk = do
    maxLabel <- go
    putStrLn . ("14b: " ++) . show $ maxLabel
    where go = do
            bmp <- GM.replicate (128,128) 0
            categorize disk bmp
            rows <- mapM (V.freeze . MM.takeRow bmp) $ [0..127]
            return . maximum . map V.maximum $ rows

isUsed :: Disk -> (Int, Int) -> Bool
isUsed disk (r,c) = testBit (disk !! r) (127 - c)

isCategorized :: Bitmap -> (Int, Int) -> IO (Bool)
isCategorized bmp idx = do
    l <- MM.read bmp idx
    return $ l /= 0

nextIndex :: (Int, Int) -> (Int, Int)
nextIndex (r,c) = ((if c == 127 then r + 1 else r), (c + 1) `mod` 128)

validIndex :: (Int, Int) -> Bool
validIndex (r,c) = (r >= 0) && (r < 128) && (c >= 0) && (c < 128)

categorize :: Disk -> Bitmap -> IO ()
categorize dsk bmp = go dsk bmp (0,0) 1
    where
        go dsk bmp idx l = when (validIndex idx) $ do
            labeled <- isCategorized bmp idx
            if isUsed dsk idx && (not labeled) then do
                MM.write bmp idx l
                processQueue dsk bmp [idx] l
                go dsk bmp (nextIndex idx) (l+1)
            else
                go dsk bmp (nextIndex idx) l

processQueue :: Disk -> Bitmap -> [(Int, Int)] -> Label -> IO ()
processQueue _ _ [] _ = return ()
processQueue dsk bmp (idx:queue) l = mapM_ go $ neighbours idx
    where
        neighbours (r,c) = nub $ [(max (r-1) 0, c)   -- N
                                 ,(r, min (c+1) 127) -- E
                                 ,(min (r+1) 127, c) -- S
                                 ,(r, max (c-1) 0)   -- W
                                 ]
        go idx' = do
            labeled <- isCategorized bmp idx'
            when (isUsed dsk idx' && (not labeled)) $ do
                MM.write bmp idx' l
                processQueue dsk bmp (idx':queue) l
