{-# LANGUAGE TypeOperators #-}

module Parser where

import Expr
import Text.ParserCombinators.Parsec

parseBool :: Parser (Expr Bool)
parseBool = (string "true" >> pure (LitBool True)) <|> (string "false" >> pure (LitBool False))

parseDisj :: Parser (Expr Bool)
parseDisj = do
    e1 <- parseExpr
    char '\\'
    char '/'
    Disj e1 <$> parseExpr

parseConj :: Parser (Expr Bool)
parseConj = do
    e1 <- parseExpr
    char '\\'
    char '/'
    Conj e1 <$> parseExpr

parseExpr :: (a ~ Bool) => Parser (Expr a)
parseExpr = parseBool <|> parseDisj <|> parseConj
