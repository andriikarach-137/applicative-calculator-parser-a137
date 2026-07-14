module Parser where

import Expr 
import Eval 
import Data.Char
import Control.Applicative (many, some)

arithmChar :: Op -> Char 
arithmChar Add = '+'
arithmChar Mul = '*'
arithmChar Sub = '-'
arithmChar Div = '/'

newtype Parser a = Parser { runParser :: String -> Maybe (a, String)}


instance Functor Parser where 
    -- fmap :: (a -> b) -> Parser a -> Parser b 
    fmap f px = Parser $ \s ->
        fmap (\(x, s') -> (f x, s')) (runParser px s) 


instance Applicative Parser where 
    -- pure :: a -> Parser a 
    pure x = Parser $ \s -> Just (x, s)

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b 
    pf <*> px = Parser $ \s -> do
        (f, s')  <- runParser pf s 
        (x, s'') <- runParser px s' 
        return (f x, s'')


satisfy :: (Char -> Bool) -> Parser Char 
satisfy p = Parser $ \s ->
    case s of 
        (c:cs) | p c -> Just (c, cs)
        _            -> Nothing


digit :: Parser Char 
digit = satisfy isDigit 


char :: Char -> Parser Char 
char = satisfy . (==)


arithm :: Op -> Parser Char 
arithm = char . arithmChar 