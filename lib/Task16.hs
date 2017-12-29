module Task16
( Move (..)
, parseMove
) where

import Control.Applicative
import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Move = Spin Int | Exchange Int Int | Partner Char Char deriving (Show, Eq)

parseMove :: String -> Move
parseMove s = case parse move "" s of
                Left e  -> error $ show e
                Right p -> p

{- Utility parsing functions -}

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

{- Grammar parsing functions -}

spin :: Parser Move
spin = do
    _ <- symbol "s"
    n <- lexeme L.decimal
    return (Spin n)

exchange :: Parser Move
exchange = do
    _ <- symbol "x"
    a <- lexeme L.decimal
    _ <- symbol "/"
    b <- lexeme L.decimal
    return (Exchange a b)

partner :: Parser Move
partner = do
    _ <- symbol "p"
    a <- lexeme L.charLiteral
    _ <- symbol "/"
    b <- lexeme L.charLiteral
    return (Partner a b)

move :: Parser Move
move = spin <|> exchange <|> partner
