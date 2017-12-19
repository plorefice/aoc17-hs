module Main where

import Lib
import qualified Data.Map as M
import qualified Data.Set as S

type ProcMap = M.Map Id Process

main :: IO ()
main = do
    input <- getContents
    let pset = S.fromList . map parseProc . lines $ input
    putStrLn . ("12a: " ++) . show . length . groupedWith (S.findMin pset) $ pset
    putStrLn . ("12b: " ++) . show . length . groups $ pset

groupedWith :: Process -> S.Set Process -> S.Set Process
groupedWith p pset = groupedWith' p pset S.empty

groups :: S.Set Process -> [S.Set Process]
groups pset = groups' pset pset

groupedWith' :: Process -> S.Set Process -> S.Set Process -> S.Set Process
groupedWith' p@(Proc id children) all pset = foldl f (S.insert p pset) $ children
    where f pset' child = let cp = S.elemAt child all
                          in if not $ S.member cp pset'
                              then groupedWith' cp all (S.insert cp pset')
                              else pset'

groups' :: S.Set Process -> S.Set Process -> [S.Set Process]
groups' all pset = if S.size pset == 0 then [] else pset' : groups' all (pset S.\\ pset')
    where pset' = groupedWith' (S.findMin pset) all S.empty
