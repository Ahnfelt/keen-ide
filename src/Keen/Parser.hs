module Keen.Parser where

import Text.Parsec hiding (State)
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.Indent
import Keen.Syntax
import Data.Maybe (maybeToList)
import Data.List (intercalate)
import Control.Monad
import Control.Monad.State

type Parser a = ParsecT String () (State SourcePos) a

parse input = runIndent "" $ runParserT p () "" input
    where
        p = do
            tokenWhitespace
            e <- parseTerm
            eof
            return e


symbols = "!@$%&/+?|^~*-.:<>"
reserved = ["=", ":", "->", "<-", "_"]


-- TOKENS

tokenWhitespace :: Parser () 
tokenWhitespace = do
    many (char ' ')
    return ()

tokenInteger :: Parser Integer
tokenInteger = do
    ds <- many1 digit
    tokenWhitespace
    return (read ds)

tokenLower' :: Parser String
tokenLower' = do
    c <- lower
    cs <- many alphaNum
    return (c:cs)

tokenLower = do r <- tokenLower'; tokenWhitespace; return r

tokenUpper' :: Parser String
tokenUpper' = do
    c <- upper
    cs <- many alphaNum
    return (c:cs)

tokenUpper = do r <- tokenUpper'; tokenWhitespace; return r

tokenSymbol' :: Parser String
tokenSymbol' = do
    s <- try $ do
        s <- many1 (oneOf symbols)
        guard (not (s `elem` reserved))
        return s
    return s

tokenSymbol = do r <- tokenSymbol'; tokenWhitespace; return r

tokenOperator :: Parser String
tokenOperator = do
    before <- optionMaybe (char '_')
    parts <- many1 (tokenSymbol' <|> tokenLower')
    after <- optionMaybe (char '_')
    tokenWhitespace
    return (maybeToList before ++ intercalate "_" parts ++ maybeToList after)

tokenReserved :: String -> Parser ()
tokenReserved s = do
    string s 
    tokenWhitespace
    return ()


-- RULES

parseInt :: Parser Term
parseInt = do
    i <- tokenInteger
    return (Int (fromIntegral i))
    
parseIdentifier :: Parser Term
parseIdentifier = do
    i <- tokenOperator
    return (Variable (Identifier i))

parseAtomic :: Parser Term
parseAtomic = parseInt <|> parseIdentifier

parseUnresolved :: Parser Term
parseUnresolved = do
    es <- many1 parseAtomic
    return (Unresolved es)
    
parseTerm = parseUnresolved <|> parseLet

parseLet = do
    ls <- withBlock' (tokenReserved "#let" >> spaces) $ do
        (x, t) <- try $ do
            x <- tokenLower
            t <- optionMaybe (tokenReserved ":" >> parseType)
            tokenReserved "="
            return (x, t)
        e <- parseTerm
        spaces
        return (Let x t e)
    e <- parseTerm
    return (LetIn ls e)

parseType = do
    x <- tokenUpper
    return (Type (Identifier x))

-- End of file
-- nice due to a bug in this IDE
-- that hides the last lines
