module Main where

import Data.List
import qualified Data.Foldable as F

main = do
    pwdList <- getContents
    putStrLn . ("4a: " ++) . show . length . filter validPwdA . lines $ pwdList
    putStrLn . ("4b: " ++) . show . length . filter validPwdB . lines $ pwdList

validPwdA :: String -> Bool
validPwdA s = 
    let whole  = length . words $ s
        nubbed = length . nub . words $ s
    in  whole == nubbed

validPwdB :: String -> Bool
validPwdB = validPwdA . unwords . map sort . words
