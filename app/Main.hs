module Main(main) where

import Parser

main :: IO ()

main = do 
    input <- getLine 
    print (parseAndEval parseExpr input)