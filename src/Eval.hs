module Eval where

import Expr 

apply :: Op -> Double -> Double -> Double 
apply Add = (+)
apply Mul = (*)
apply Sub = (-)
apply Div = (/)

eval :: Expr -> Double 
eval (Val n) = n
eval (BinOp Div _ 0) = undefined 
eval (BinOp op e1 e2) = apply op $ op1 op2  
eval _ = undefined 