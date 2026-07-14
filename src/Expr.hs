module Expr where

data Op 
    = Add 
    | Mul 
    | Sub
    | Div

data Expr 
    = BinOp Op Expr Expr 
    | Val Double 