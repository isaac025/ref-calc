module Parser where

import Control.Monad (void)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Vector (Vector)
import Data.Vector qualified as V
import Text.ParserCombinators.Parsec

data BinOp
    = Add
    | Sub
    | Mult
    | Div
    | And
    | Or
    | XOr
    | Impl
    | Equiv
    | Gt
    | GtE
    | Lt
    | LtE
    | Equal
    deriving (Show, Ord, Eq)

data Expr a
    = IntE Int
    | BoolE Bool
    | ArrE (Vector (Expr a))
    | SetE (Set (Expr a))
    | BinOpE BinOp (Expr a) (Expr a)
    | CndE (Expr a) (Expr a) (Expr a)
    deriving (Show, Eq, Ord)

-- Helpers --
whitespace :: Parser ()
whitespace =
    choice
        [ simpleWhitespace *> whitespace
        , lineComment *> whitespace
        , return ()
        ]
  where
    lineComment =
        try (string "--")
            *> manyTill anyChar (void (char '\n') <|> eof)
    simpleWhitespace = void $ many1 (oneOf " \t\n")

parens :: Parser a -> Parser a
parens = between (lexeme (char '(')) (lexeme (char ')'))

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

-- Helpers --

int :: Parser Int
int = read <$> lexeme (many1 digit)

num :: Parser (Expr a)
num = IntE <$> int

bool :: Parser (Expr a)
bool = (BoolE True <$ lexeme (string "true")) <|> (BoolE False <$ lexeme (string "false"))

array :: Parser (Expr a)
array = do
    elements <- between (lexeme (char '[')) (lexeme (char ']')) (term `sepBy` lexeme (char ','))
    pure $ ArrE (V.fromList elements)

set :: Parser (Expr a)
set = do
    elements <- between (lexeme (char '{')) (lexeme (char '}')) (term `sepBy` lexeme (char ','))
    pure $ SetE (S.fromList elements)

term :: Parser (Expr a)
term = num <|> bool <|> array <|> set <|> parens expr

multOp :: Parser (Expr a -> Expr a -> Expr a)
multOp = lexeme (char '*') *> pure (BinOpE Mult)

divOp :: Parser (Expr a -> Expr a -> Expr a)
divOp = lexeme (char '/') *> pure (BinOpE Div)

addOp :: Parser (Expr a -> Expr a -> Expr a)
addOp = lexeme (char '+') *> pure (BinOpE Add)

subOp :: Parser (Expr a -> Expr a -> Expr a)
subOp = lexeme (char '-') *> pure (BinOpE Sub)

factor :: Parser (Expr a)
factor = chainl1 term (multOp <|> divOp)

arithExpr :: Parser (Expr a)
arithExpr = chainl1 factor (addOp <|> subOp)

implOp :: Parser (Expr a -> Expr a -> Expr a)
implOp = lexeme (string "=>") *> pure (BinOpE Impl)

equivOp :: Parser (Expr a -> Expr a -> Expr a)
equivOp = lexeme (string "<=>") *> pure (BinOpE Equiv)

andOp :: Parser (Expr a -> Expr a -> Expr a)
andOp = lexeme (char '&') *> pure (BinOpE And)

orOp :: Parser (Expr a -> Expr a -> Expr a)
orOp = lexeme (char '|') *> pure (BinOpE Or)

xorOp :: Parser (Expr a -> Expr a -> Expr a)
xorOp = lexeme (char '^') *> pure (BinOpE XOr)

eqOp :: Parser (Expr a -> Expr a -> Expr a)
eqOp = lexeme (char '=') *> pure (BinOpE Equal)

gtOp :: Parser (Expr a -> Expr a -> Expr a)
gtOp = lexeme (char '>') *> pure (BinOpE Gt)

gtEOp :: Parser (Expr a -> Expr a -> Expr a)
gtEOp = lexeme (string ">=") *> pure (BinOpE GtE)

ltOp :: Parser (Expr a -> Expr a -> Expr a)
ltOp = lexeme (char '<') *> pure (BinOpE Lt)

ltEOp :: Parser (Expr a -> Expr a -> Expr a)
ltEOp = lexeme (string "<=") *> pure (BinOpE LtE)

relExpr :: Parser (Expr a)
relExpr = chainl1 arithExpr (ltOp <|> gtOp <|> gtEOp <|> ltEOp <|> eqOp)

boolExpr :: Parser (Expr a)
boolExpr = chainl1 relExpr (andOp <|> xorOp <|> orOp)

propExpr :: Parser (Expr a)
propExpr = chainr1 boolExpr implOp

ifExpr :: Parser (Expr a)
ifExpr = do
    _ <- lexeme (string "IF")
    cond <- lexeme expr
    _ <- lexeme (string "THEN")
    thenExpr <- lexeme expr
    _ <- lexeme (string "ELSE")
    elseExpr <- lexeme expr
    pure (CndE cond thenExpr elseExpr)

expr :: Parser (Expr a)
expr = propExpr <|> array <|> set <|> ifExpr

parseExpr :: String -> Either String (Expr a)
parseExpr input =
    case parse expr "RC" input of
        Left err -> Left $ "Parsing error: " ++ show err
        Right val -> Right val
