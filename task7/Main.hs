module Main where

import Task7

import Control.Monad
import Data.List
import qualified Data.Map as M

data Tree = Nil | Tree Proc [Tree] deriving (Show, Eq)

main :: IO ()
main = do
    input <- getContents
    let procs = procList . lines $ input
    putStrLn . ("7a: " ++) . root . build $ procs
    putStrLn . ("7b: " ++) . show . adjustedWeight . build $ procs

procList :: [String] -> [Proc]
procList = map parseProc

root :: Tree -> String
root (Tree (Proc n _ _) _) = n

build :: [Proc] -> Tree
build pl = build' pl Nil

build' :: [Proc] -> Tree -> Tree
build' [] t = t
build' (p:pl) t =
    case insert' p t of
        Left t'  -> build' (pl ++ [p]) t'
        Right t' -> build' pl t'

insert' :: Proc -> Tree -> Either Tree Tree
insert' p Nil = Right $ Tree p []
insert' p@(Proc pn _ pc) t@(Tree p'@(Proc tn _ tc) tl)
    | tn `elem` pc = Right $ Tree p [t]
    | pn `elem` tc = Right $ Tree p' ((Tree p []) : tl)
    | tl == []     = Left t
    | otherwise    =
        let folder acc (idx, t') = case insert' p t' of
                                    Left _    -> acc
                                    Right t'' -> (Just idx, t'')
            attempt = foldl folder (Nothing, Nil) $ zip [0..] tl
        in  case attempt of
                (Nothing, _)   -> Left t
                (Just idx, t') -> Right $ Tree p' (take idx tl ++ (t' : (drop (idx + 1) tl)))

adjustedWeight :: Tree -> Int
adjustedWeight t =
    let ((Tree _ children), target) = findUnbalanced t 0
        childrenWeight = sum . map weight $ children
    in  target - childrenWeight

weight :: Tree -> Int
weight (Tree (Proc _ w _) tl) = w + (sum . map weight $ tl)

findUnbalanced :: Tree -> Int -> (Tree, Int)
findUnbalanced t@(Tree p@(Proc _ w _) tl) target =
    let weights    = map weight tl
        indexed    = M.fromListWith (++) . zip weights . map (:[]) $ [0..]
    in  if length indexed == 1
        then (t, target)
        else let unbalanced = tl !! (head . snd . M.elemAt 0 . M.filter (\l -> length l == 1) $ indexed)
                 target'    = head . M.keys . M.filter (\l -> length l > 1) $ indexed
             in  findUnbalanced unbalanced target'
