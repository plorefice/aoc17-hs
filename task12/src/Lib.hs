module Lib
( Process (..), Id
, parseProc
) where


import Control.Applicative
import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

type Id = Int
data Process = Proc Id [Id] deriving (Show)

instance Eq Process where
    (Proc id _) == (Proc id' _) = id == id'

instance Ord Process where
    (Proc id _) `compare` (Proc id' _) = id `compare` id'

parseProc :: String -> Process
parseProc s = case parse proc "" s of
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

pid :: Parser Id
pid = lexeme L.decimal

proc :: Parser Process
proc = do
    id <- pid
    _  <- symbol "<->"
    cl <- sepBy1 pid (symbol ",")
    return (Proc id cl)
