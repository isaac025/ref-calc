module Parser where

import Control.Monad (void)
import Text.ParserCombinators.Parsec

data Expr a
    = LitBool Bool
    | LitInt Int
    | LitIden String
    | DefIden String String
    | UnaOp String (Expr a)
    | BinOp (Expr a) String (Expr a)

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

-- Helpers --

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

int :: Parser Int
int = read <$> lexeme (many1 digit)

iden :: Parser String
iden = lexeme ((:) <$> firstChar <*> many nonFirstChar)
  where
    firstChar :: Parser Char
    firstChar = letter <|> char '_'

    nonFirstChar :: Parser Char
    nonFirstChar = digit <|> firstChar

num :: Parser (Expr a)
num = LitInt <$> int

boolT :: Parser (Expr a)
boolT = string "true" >> pure (LitBool True)

boolF :: Parser (Expr a)
boolF = string "false" >> pure (LitBool False)

var :: Parser (Expr a)
var = LitIden <$> iden

term :: Parser (Expr a)
term = num <|> boolT <|> boolF <|> var
