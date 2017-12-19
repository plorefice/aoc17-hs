module Lib
( Direction (..)
, parseDir
) where

import Control.Applicative
import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Direction = N | S | NE | NW | SE | SW deriving (Show, Eq)

type Parser = Parsec Void String

parseDir :: String -> Direction
parseDir s = case parse dir "" s of
                Left e  -> error $ show e
                Right d -> d

{- Utility parsing functions -}

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

{- Grammar parsing functions -}

dir :: Parser Direction
dir = (symbol "ne" *> pure NE)
    <|> (symbol "nw" *> pure NW)
    <|> (symbol "se" *> pure SE)
    <|> (symbol "sw" *> pure SW)
    <|> (symbol "n" *> pure N)
    <|> (symbol "s" *> pure S)
