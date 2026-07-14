module Expr where

data Expr 
    = Add Expr Expr
    | Mul Expr Expr 
    | Sub Expr Expr 
    | Val Double 