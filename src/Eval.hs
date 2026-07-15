module Eval where

import Expr 


apply :: Op -> Int -> Int -> Maybe Int 
apply Add = curry $ Just . uncurry (+)
apply Mul = curry $ Just . uncurry (*)
apply Sub = curry $ Just . uncurry (-)
apply Div = safeDiv 


safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing 
safeDiv n m = Just $ div n m  


eval :: Expr -> Maybe Int
eval (Val n) = Just n 
eval (BinOp op e1 e2) = 
    eval e1 >>= \x -> 
    eval e2 >>= \y ->
    apply op x y