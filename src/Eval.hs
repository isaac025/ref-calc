module Eval where

import Data.Set (Set, elemAt, fromList, null, toList)
import Data.Vector (Vector, uncons)
import Parser
import Prelude hiding (null)

data Value
    = IntVal Int
    | BoolVal Bool
    | ArrVal (Vector Value)
    | SetVal (Set Value)
    deriving (Eq, Ord)

instance Show Value where
    show (IntVal i) = show i
    show (BoolVal b) = show b
    show (SetVal s) = show s
    show (ArrVal v) = show v

data Type
    = IntT
    | BoolT
    | ArrT (Maybe Type)
    | SetT (Maybe Type)
    deriving (Eq)

instance Show Type where
    show IntT = "Int64"
    show BoolT = "Bool"
    show (ArrT Nothing) = "Array"
    show (ArrT (Just a)) = "Array of " <> show a
    show (SetT Nothing) = "Set"
    show (SetT (Just a)) = "Set of " <> show a

tc :: Expr a -> Type
tc (LitInt _) = IntT
tc (LitBool _) = BoolT
tc (LitSet s)
    | null s = SetT Nothing
    | otherwise =
        let t = elemAt 1 s
         in SetT (Just (tc t))
tc (LitArr v) =
    case uncons v of
        Nothing -> ArrT Nothing
        Just (e, _) -> ArrT $ Just (tc e)
{-
tc (Cnd c t e) =
    let ct = tc c
     in if BoolT == ct
            then
                if tc t == tc e
                    then tc t
                    else error "type mismatch at IF"
            else error $ "IF expects Bool type not: " <> show ct
-}
tc (BinOpE o e1 e2) =
    case o of
        Add ->
            if tc e1 == IntT && tc e2 == IntT
                then IntT
                else error "+ expects Int64"
        Sub ->
            if tc e1 == IntT && tc e2 == IntT
                then IntT
                else error "- expects Int64"
        Mult ->
            if tc e1 == IntT && tc e2 == IntT
                then IntT
                else error "* expects Int64"
        Div ->
            if tc e1 == IntT && tc e2 == IntT
                then IntT
                else error "/ expects Int64"
        And ->
            if tc e1 == BoolT && tc e2 == BoolT
                then BoolT
                else error "& expects Bool"
        Or ->
            if tc e1 == BoolT && tc e2 == BoolT
                then BoolT
                else error "| expects Bool"
        XOr ->
            if tc e1 == BoolT && tc e2 == BoolT
                then BoolT
                else error "^ expects Bool"
        Impl ->
            if tc e1 == BoolT && tc e2 == BoolT
                then BoolT
                else error "==> expects Bool"
        Equal ->
            if tc e1 == BoolT && tc e2 == BoolT
                then BoolT
                else error "<==> expects Bool"

eval :: Expr a -> Value
eval (LitInt i) = IntVal i
eval (LitBool b) = BoolVal b
{-eval (Cnd c t e) =
    if boolDecision (eval c)
        then eval t
        else eval e
-}
eval (LitArr v) = ArrVal (fmap eval v)
eval (LitSet s) = SetVal (fromList $ map eval $ toList s)
eval (BinOpE b e1 e2) =
    let o = binaryDecision b
     in o (eval e1) (eval e2)

boolDecision :: Value -> Bool
boolDecision (BoolVal b) = b
boolDecision _ = error "IF expects boolean"

binaryDecision :: BinOp -> (Value -> Value -> Value)
binaryDecision Add = add'
binaryDecision Sub = sub'
binaryDecision Mult = mult'
binaryDecision Div = div'
binaryDecision And = and'
binaryDecision Or = or'
binaryDecision XOr = xor'
binaryDecision Impl = impl'
binaryDecision Equal = eq'

add' :: Value -> Value -> Value
add' (IntVal x) (IntVal y) = IntVal (x + y)
add' _ _ = error "+ should only match on Int64"

sub' :: Value -> Value -> Value
sub' (IntVal x) (IntVal y) = IntVal (x - y)
sub' _ _ = error "- should only match on Int64"

mult' :: Value -> Value -> Value
mult' (IntVal x) (IntVal y) = IntVal (x * y)
mult' _ _ = error "* should only match on Int64"

div' :: Value -> Value -> Value
div' (IntVal x) (IntVal y) = IntVal (x `div` y)
div' _ _ = error "/ should only match on Int64"

and' :: Value -> Value -> Value
and' (BoolVal x) (BoolVal y) = BoolVal (x && y)
and' _ _ = error "& should only match on Bool"

or' :: Value -> Value -> Value
or' (BoolVal x) (BoolVal y) = BoolVal (x || y)
or' _ _ = error "| should only match on Bool"

xor' :: Value -> Value -> Value
xor' (BoolVal x) (BoolVal y) = BoolVal ((x && not y) || (not x && y))
xor' _ _ = error "^ should only match on Bool"

impl' :: Value -> Value -> Value
impl' (BoolVal x) (BoolVal y) = BoolVal (not x || y)
impl' _ _ = error "==> should only match on Bool"

eq' :: Value -> Value -> Value
eq' (BoolVal x) (BoolVal y) = BoolVal (x == y)
eq' _ _ = error "<==> should only match on Bool"
