module Main where

import Lib

import Control.Monad
import Data.List

data Tree = Nil | Tree Proc [Tree] deriving (Show, Eq)

main :: IO ()
main = do
    input <- getContents
    let procs = procList . lines $ input
    putStrLn . ("7a: " ++) . root . build $ procs

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
