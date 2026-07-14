module Eval where

import Expr 


apply :: Op -> Double -> Double -> Maybe Double 
apply Add = curry $ Just . uncurry (+)
apply Mul = curry $ Just . uncurry (*)
apply Sub = curry $ Just . uncurry (-)
apply Div = safeDiv 


safeDiv :: Double -> Double -> Maybe Double 
safeDiv _ 0 = Nothing 
safeDiv n m = Just $ n / m 


eval :: Expr -> Maybe Double 
eval (Val n) = Just n 
eval (BinOp op e1 e2) = 
    eval e1 >>= \x -> 
    eval e2 >>= \y ->
    apply op x y
