module Task20
( Vec3
, Particle
, parseParticle
) where

import Control.Applicative
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

type Vec3 = (Int, Int, Int)

data Particle
  = Particle
  { pos :: Vec3
  , vel :: Vec3
  , acc :: Vec3
  } deriving (Show)

parseParticle :: String -> Particle
parseParticle s = case parse particle "" s of
                    Right p -> p
                    Left  e -> error $ show e

{- Utility parsing functions -}

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

{- Grammar parsing functions -}

value :: Parser Int
value = let integer = lexeme L.decimal in L.signed sc integer

vec3 :: Parser Vec3
vec3 = do
  symbol "<"
  x <- value
  symbol ","
  y <- value
  symbol ","
  z <- value
  symbol ">"
  return $ (x,y,z)

particle :: Parser Particle
particle = do
  symbol "p="
  p <- vec3
  symbol ", v="
  v <- vec3
  symbol ", a="
  a <- vec3
  return $ Particle { pos = p, vel = v, acc = a }
