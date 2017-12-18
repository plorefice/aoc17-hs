module Lib
( Proc (..)
, parseProc
) where

import Control.Applicative
import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Proc = Proc String Int [String] deriving (Show, Eq)

type Parser = Parsec Void String

parseProc :: String -> Proc
parseProc s =
    case parse whileParser "" s of
        Left e  -> error $ show e
        Right r -> r

{- Utility parsing functions -}

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

whileParser :: Parser Proc
whileParser = between sc eof proc

symbol :: String -> Parser String
symbol = L.symbol sc

integer :: Parser Int
integer = lexeme L.decimal

name :: Parser String
name = lexeme $ (:) <$> letterChar <*> many letterChar

proc :: Parser Proc
proc = do
    pn <- name
    pw <- between (symbol "(") (symbol ")") $ lexeme L.decimal
    pc <- option [] (do { _ <- symbol "->"; name `sepBy1` (symbol ",")})
    return (Proc pn pw pc)
