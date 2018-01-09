module Main where

import Data.List
import Data.Function (on)
import Data.Foldable (toList)
import Data.Maybe (isJust)
import Data.List.Split (splitOn)
import qualified Data.Sequence as S

type Block  = (Int, Int)
type Bridge = S.Seq Block

main :: IO ()
main = do
  input <- getContents
  let bridges = build . S.fromList . map parseBlock . lines $ input
  putStrLn . ("24a: " ++) . show . maximum . map strength $ bridges
  putStrLn . ("24b: " ++) . show . head . reverse . sort $ zip (map S.length bridges) (map strength bridges)

parseBlock :: String -> Block
parseBlock = (\(a:b:[]) -> (read a, read b)) . splitOn "/"

build :: S.Seq Block -> [Bridge]
build blks = concat
           . toList
           . S.mapWithIndex (\_ (b, blks) -> build' blks (S.fromList [b]))
           . S.filter ((== 0) . fst . fst)
           . S.mapWithIndex (\i b -> (b, S.deleteAt i blks))
           . S.mapWithIndex (\_ (l,r) -> if r == 0 then (r,l) else (l,r))
           $ blks

build' :: S.Seq Block -> Bridge -> [Bridge]
build' blks br = concatMap (\(Just (br', blks')) -> br' : build' blks' br')
               . filter isJust
               . toList
               . S.mapWithIndex f
               $ blks
  where f idx blk = case attach blk br of
                      Just br' -> Just (br', S.deleteAt idx blks)
                      Nothing  -> Nothing

attach :: Block -> Bridge -> Maybe (Bridge)
attach (l,r) br
  | S.length br == 0 = Just $ S.fromList [(l,r)]
  | (snd last ) == l = Just $ br S.|> (l,r)
  | (snd last ) == r = Just $ br S.|> (r,l)
  | otherwise = Nothing
  where first = S.index br 0
        last  = S.index br (S.length br - 1)

strength :: Bridge -> Int
strength = foldl (\s (l,r) -> s + l + r) 0
