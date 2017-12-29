module Task18
( Instr (..)
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
  | IntConst Int
  deriving (Show, Eq)

data Instr
  = Snd Register
  | Set Register Expr
  | Add Register Expr
  | Mul Register Expr
  | Mod Register Expr
  | Rcv Register
  | Jgz Register Expr
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
expr = (IntConst <$> value) <|> (Reg <$> reg)

sound :: Parser Instr
sound = do
    symbol "snd"
    r <- reg
    return $ Snd r

set :: Parser Instr
set = do
    symbol "set"
    r <- reg
    e <- expr
    return $ Set r e

add :: Parser Instr
add = do
    symbol "add"
    r <- reg
    e <- expr
    return $ Add r e

mul :: Parser Instr
mul = do
    symbol "mul"
    r <- reg
    e <- expr
    return $ Mul r e

modul :: Parser Instr
modul = do
    symbol "mod"
    r <- reg
    e <- expr
    return $ Mod r e

rcv :: Parser Instr
rcv = do
    symbol "rcv"
    r <- reg
    return $ Rcv r

jgz :: Parser Instr
jgz = do
    symbol "jgz"
    r <- reg
    e <- expr
    return $ Jgz r e

instr :: Parser Instr
instr = sound <|> set <|> add <|> mul <|> modul <|> rcv <|> jgz
