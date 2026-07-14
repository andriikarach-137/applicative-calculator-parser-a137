module Expr where

data Op 
    = Add 
    | Mul 
    | Sub

data Expr 
    = BinOp Op Expr Expr 
    | Val Double 