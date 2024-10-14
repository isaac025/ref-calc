{-# LANGUAGE GADTs #-}

module Expr where

data Expr a where
    LitBool :: Bool -> Expr Bool
    Conj :: Expr Bool -> Expr Bool -> Expr Bool
    Disj :: Expr Bool -> Expr Bool -> Expr Bool
    Nega :: Expr Bool -> Expr Bool
    Impl :: Expr Bool -> Expr Bool -> Expr Bool
    Equi :: Expr Bool -> Expr Bool -> Expr Bool

instance (Show a) => Show (Expr a) where
    show (LitBool b) = show b
    show (Conj e1 e2) = show e1 <> "/\\" <> show e2
    show (Disj e1 e2) = show e1 <> "\\/" <> show e2
    show (Nega b) = show "~" <> show b
    show (Impl p q) = show p <> " ==> " <> show q
    show (Equi p q) = show p <> " <==> " <> show q

eval :: Expr a -> a
eval (LitBool b) = b
eval (Conj e1 e2) = eval e1 && eval e2
eval (Disj e1 e2) = eval e1 || eval e2
eval (Nega b) = not $ eval b
eval (Impl p q) = not (eval p) || eval q
eval (Equi p q) = eval p == eval q
