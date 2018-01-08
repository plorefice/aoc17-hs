module Task23
( Instr (..), Register, Expr (..)
, parseInstr
) where

import Control.Applicative
import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Sequence as S

type Parser = Parsec Void String

type Register = Char

data Expr
  = Reg Register
  | Val Int
  deriving (Show, Eq)

data Instr
  = Set Register Expr
  | Sub Register Expr
  | Mul Register Expr
  | Jnz Expr Expr
  deriving (Show, Eq)

parseInstr :: String -> Instr
parseInstr s =
    case parse instr "" s of
        Right i -> i
        Left  e -> error $ show e

{- Grammar utility functions -}

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

{- Grammar parsing functions -}

reg :: Parser Register
reg = lexeme L.charLiteral

value :: Parser Int
value = L.signed sc $ lexeme L.decimal

expr :: Parser Expr
expr = (Val <$> value) <|> (Reg <$> reg)

set :: Parser Instr
set = do
    symbol "set"
    r <- reg
    e <- expr
    return $ Set r e

sub :: Parser Instr
sub = do
    symbol "sub"
    r <- reg
    e <- expr
    return $ Sub r e

mul :: Parser Instr
mul = do
    symbol "mul"
    r <- reg
    e <- expr
    return $ Mul r e

jnz :: Parser Instr
jnz = do
    symbol "jnz"
    e1 <- expr
    e2 <- expr
    return $ Jnz e1 e2

instr :: Parser Instr
instr = set <|> sub <|> mul <|> jnz
