module Parser where

import Control.Monad (void)
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
    | Equal
    deriving (Show, Eq)

data Expr a
    = LitInt Int
    | LitBool Bool
    | BinOpE BinOp (Expr a) (Expr a)
    | Cnd (Expr a) (Expr a) (Expr a)
    deriving (Show)

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
num = LitInt <$> int

bool :: Parser (Expr a)
bool = (LitBool True <$ lexeme (string "true")) <|> (LitBool False <$ lexeme (string "false"))

term :: Parser (Expr a)
term = num <|> bool <|> parens expr

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
implOp = lexeme (string "==>") *> pure (BinOpE Impl)

eqOp :: Parser (Expr a -> Expr a -> Expr a)
eqOp = lexeme (string "<==>") *> pure (BinOpE Equal)

andOp :: Parser (Expr a -> Expr a -> Expr a)
andOp = lexeme (char '&') *> pure (BinOpE And)

orOp :: Parser (Expr a -> Expr a -> Expr a)
orOp = lexeme (char '|') *> pure (BinOpE Or)

xorOp :: Parser (Expr a -> Expr a -> Expr a)
xorOp = lexeme (char '^') *> pure (BinOpE XOr)

propExpr :: Parser (Expr a)
propExpr = chainl1 arithExpr (andOp <|> xorOp <|> orOp)

boolExpr :: Parser (Expr a)
boolExpr = chainl1 propExpr (implOp <|> eqOp)

expr :: Parser (Expr a)
expr = boolExpr

parseExpr :: String -> Either String (Expr a)
parseExpr input =
    case parse expr "RC" input of
        Left err -> Left $ "Parsing error: " ++ show err
        Right val -> Right val
