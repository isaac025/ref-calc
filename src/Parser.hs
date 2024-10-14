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
    deriving (Eq)

data Expr a
    = LitInt Int
    | LitBool Bool
    | BinOpE BinOp (Expr a) (Expr a)
    | Cnd (Expr a) (Expr a) (Expr a)

-- Helpers --
whitespace :: Parser ()
whitespace =
    choice
        [ simpleWhitespace *> whitespace
        , lineComment *> whitespace
        , blockComment *> whitespace
        , return ()
        ]
  where
    lineComment =
        try (string "//")
            *> manyTill anyChar (void (char '\n') <|> eof)
    blockComment =
        try (string "/**")
            *> manyTill anyChar (try $ string "*/")
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

andOp :: Parser (Expr a -> Expr a -> Expr a)
andOp = lexeme (string "/\\") *> pure (BinOpE And)

orOp :: Parser (Expr a -> Expr a -> Expr a)
orOp = lexeme (string "/\\") *> pure (BinOpE Or)

boolExpr :: Parser (Expr a)
boolExpr = chainl1 arithExpr (andOp <|> orOp)

expr :: Parser (Expr a)
expr = boolExpr

parseExpr :: String -> Either String (Expr a)
parseExpr input =
    case parse expr "RC" input of
        Left err -> Left $ "Parsing error: " ++ show err
        Right val -> Right val
