module Eval where

import Parser

data Value = IntVal Int | BoolVal

data Type = IntT | BoolT
    deriving (Eq)

instance Show Type where
    show IntT = "Int64"
    show BoolT = "Bool"

tc :: Expr a -> Type
tc (LitInt _) = IntT
tc (LitBool _) = BoolT
tc (Cnd c t e) =
    let ct = tc c
     in if BoolT == ct
            then
                if tc t == tc e
                    then tc t
                    else error "type mismatch at IF"
            else error $ "IF expects Bool type not: " <> show ct
tc (BinOpE o e1 e2) =
    case o of
        Add ->
            if tc e1 == IntT && tc e2 == IntT
                then IntT
                else error "+ expects number"
        Sub ->
            if tc e1 == IntT && tc e2 == IntT
                then IntT
                else error "- expects number"
        Mult ->
            if tc e1 == IntT && tc e2 == IntT
                then IntT
                else error "* expects number"
        Div ->
            if tc e1 == IntT && tc e2 == IntT
                then IntT
                else error "/ expects number"
        And ->
            if tc e1 == BoolT && tc e2 == BoolT
                then BoolT
                else error "+ expects number"
        Or ->
            if tc e1 == BoolT && tc e2 == BoolT
                then BoolT
                else error "+ expects number"
        XOr ->
            if tc e1 == BoolT && tc e2 == BoolT
                then BoolT
                else error "+ expects number"
