module Expr where

data Op 
    = Add 
    | Mul 
    | Sub
    | Div
    deriving Show

data Expr 
    = BinOp Op Expr Expr 
    | Val Int
    deriving Show