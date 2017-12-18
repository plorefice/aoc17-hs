module Lib
( Instr
, parseInstr
) where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Register = String
type Value    = Int
data ArithOp  = Inc | Dec deriving (Show)
data BoolOp   = EQ | NE | LT | GT | LE | GE deriving (Show)
data Cond     = Cond Register BoolOp Value deriving (Show)
data Instr    = Instr Register ArithOp Value Cond deriving (Show)

type Parser = Parsec Void String

parseInstr :: String -> Instr
parseInstr = error
