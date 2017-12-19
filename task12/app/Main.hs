module Main where

import Lib
import qualified Data.Map as M
import qualified Data.Set as S

type ProcMap = M.Map Id Process

main :: IO ()
main = do
    input <- getContents
    let pmap = M.fromList . map (\p@(Proc id _) -> (id, p)) . map parseProc . lines $ input
    putStrLn . ("12a: " ++) . show . length . groupedWith (pmap M.! 0) $ pmap

groupedWith :: Process -> ProcMap -> [Id]
groupedWith p pm = S.toList . groupedWith' p pm $ S.empty

groupedWith' :: Process -> ProcMap -> S.Set Id -> S.Set Id
groupedWith' (Proc id children) pmap pset = foldl f (S.insert id pset) $ children
    where f pset' child = if not $ S.member child pset'
                  then groupedWith' (pmap M.! child) pmap (S.insert child pset')
                  else pset'
