module Task8
( Register, Value , AExpr (..), BExpr (..) , ArithOp (..) , BoolOp (..), Instr (..)
, parseInstr
) where

import Control.Applicative
import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L

type Register = String
type Value    = Int
data AExpr    = ABinary ArithOp Register Value deriving (Show)
data BExpr    = BBinary BoolOp Register Value deriving (Show)
data ArithOp  = Inc | Dec deriving (Show)
data BoolOp   = EQ | NE | LT | GT | LE | GE deriving (Show)
data Instr    = Instr AExpr BExpr deriving (Show)

type Parser = Parsec Void String

parseInstr :: String -> Instr
parseInstr s =
    case parse instr "" s of
        Left e  -> error $ show e
        Right i -> i

{- Utility parsing functions -}

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

{- Grammar parsing functions -}

reg :: Parser Register
reg = lexeme $ (:) <$> letterChar <*> many letterChar

value :: Parser Int
value = let integer = lexeme L.decimal in L.signed sc integer

arithOp :: Parser ArithOp
arithOp = (symbol "inc" *> pure Inc)
    <|> (symbol "dec" *> pure Dec)

boolOp :: Parser BoolOp
boolOp = (symbol "==" *> pure Task8.EQ)
    <|> (symbol "!=" *> pure Task8.NE)
    <|> (symbol "<=" *> pure Task8.LE)
    <|> (symbol ">=" *> pure Task8.GE)
    <|> (symbol "<"  *> pure Task8.LT)
    <|> (symbol ">"  *> pure Task8.GT)

aExpr :: Parser AExpr
aExpr = do
    r <- reg
    op <- arithOp
    v <- value
    return (ABinary op r v)

bExpr :: Parser BExpr
bExpr = do
    r <- reg
    op <- boolOp
    v <- value
    return (BBinary op r v)

instr :: Parser Instr
instr = do
    expr <- aExpr
    _    <- symbol "if"
    cond <- bExpr
    return (Instr expr cond)
