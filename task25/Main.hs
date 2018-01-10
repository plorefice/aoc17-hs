module Main where

import Debug.Trace
import Data.List
import Data.Char
import qualified Data.HashTable.IO as H

type Direction
  = (Int -> Int -> Int)

data Operations
  = Operations Int Direction Int

data State
  = State Operations Operations

data Blueprint
  = Blueprint
  { initialState :: Int
  , steps        :: Int
  , states       :: [State]
  }

type Tape
  = H.BasicHashTable Int Int

data Program
  = Program
  { tape      :: Tape
  , cursor    :: Int
  , remSteps  :: Int
  , st        :: State
  , blueprint :: Blueprint
  }

sample :: Blueprint
sample = Blueprint { initialState = 0
                   , steps        = 6
                   , states       = [ State
                                        ( Operations 1 (+) 1 )
                                        ( Operations 0 (-) 1 )
                                    , State
                                        ( Operations 1 (-) 0 )
                                        ( Operations 1 (+) 0 )
                                    ]
                   }

input :: Blueprint
input = Blueprint { initialState = 0
                  , steps        = 12172063
                  , states       = [ State
                                        ( Operations 1 (+) 1 )
                                        ( Operations 0 (-) 2 )
                                    , State
                                        ( Operations 1 (-) 0 )
                                        ( Operations 1 (-) 3 )
                                    , State
                                        ( Operations 1 (+) 3 )
                                        ( Operations 0 (+) 2 )
                                    , State
                                        ( Operations 0 (-) 1 )
                                        ( Operations 0 (+) 4 )
                                    , State
                                        ( Operations 1 (+) 2 )
                                        ( Operations 1 (-) 5 )
                                    , State
                                        ( Operations 1 (-) 4 )
                                        ( Operations 1 (+) 0 )
                                    ]
                  }

main :: IO ()
main = do
  p <- blank input >>= execute
  t <- H.toList . tape $ p
  putStrLn . ("25a: " ++) . show . sum . map snd . sort $ t

blank :: Blueprint -> IO (Program)
blank bp = do
  ht <- H.new
  return $ Program { tape      = ht
                   , cursor    = 0
                   , remSteps  = steps bp
                   , st        = states bp !! (initialState bp)
                   , blueprint = bp
                   }

execute :: Program -> IO (Program)
execute p @ Program { remSteps = 0 } = return p
execute p = step p >>= execute

step :: Program -> IO (Program)
step p @ Program { tape      = t
                 , cursor    = c
                 , remSteps  = rs
                 , st        = (State op0 op1)
                 , blueprint = bp
                 } =
  do
    val' <- tapeAt t c
    let (Operations val dir next) = if val' == 0 then op0 else op1
    H.insert t c val
    return $ p { cursor    = c `dir` 1
                , remSteps  = rs - 1
                , st        = states bp !! next
                }

tapeAt :: Tape -> Int -> IO (Int)
tapeAt t c = do
  v <- H.lookup t c
  case v of Just v  -> return v
            Nothing -> H.insert t c 0 >> return 0