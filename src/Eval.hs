module Eval where

import Expr 
import Control.Applicative (liftA2)


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
eval (BinOp op e1 e2) = liftA2 (apply op) (eval e1) (eval e2)