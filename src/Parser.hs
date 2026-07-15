module Parser where

import Expr 
import Eval 
import Data.Char
import Data.Functor
import Control.Applicative (liftA3, Alternative(..), many, some)

arithmChar :: Char -> Op
arithmChar '+' = Add 
arithmChar '*' = Mul 
arithmChar '-' = Sub 
arithmChar '/' = Div 
 
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


instance Alternative Parser where 
    -- empty :: Parser a
    empty = Parser $ \_ -> Nothing

    -- (<|>) :: Parser a -> Parser a -> Parser a
    (<|>) px py = Parser $ \s -> 
        case runParser px s of
            Nothing -> runParser py s 
            result  -> result


satisfy :: (Char -> Bool) -> Parser Char 
satisfy p = Parser $ \s ->
    case s of 
        (c:cs) | p c -> Just (c, cs)
        _            -> Nothing


digit :: Parser Char 
digit = satisfy isDigit 


char :: Char -> Parser Char 
char = satisfy . (==)


spaces :: Parser Char
spaces = many (char ' ') $> ' ' 


number :: Parser Int 
number = read <$> some digit


parseValExpr :: Parser Expr 
parseValExpr = Val <$> number 


-- BinOp Expr is either ( e1 op e2) or e1 op e2
parseBinOpExpr :: Parser Expr 
parseBinOpExpr =
    char '(' *> liftA3 (\e1 op e2 -> BinOp op e1 e2) (parseExpr <* spaces) (parseOp <* spaces) parseExpr <* char ')'


parseExpr :: Parser Expr
parseExpr = parseBinOpExpr <|> parseValExpr


parseOp :: Parser Op
parseOp = arithmChar <$> (char '+' <|> char '*' <|> char '-' <|> char '/')


parseAndEval :: Parser Expr -> String -> Maybe Int 
parseAndEval p s =
    runParser p s >>= \(expr, _) -> eval expr 