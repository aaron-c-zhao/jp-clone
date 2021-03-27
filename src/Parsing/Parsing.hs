{-# OPTIONS_GHC -w #-}
-- Functional parsing library from chapter 13 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.

module Parsing.Parsing (module Parsing.Parsing, module Control.Applicative) where

import Control.Applicative
import Data.Char

-- Basic definitions

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

-- Sequencing parsers

instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   -- applys g on the output 
   fmap g p = P (\inp -> case parse p inp of
                            []        -> []
                            [(v,out)] -> [(g v, out)])

instance Applicative Parser where
   -- pure :: a -> Parser a
   -- wrap a val v into a Parser
   pure v = P (\inp -> [(v,inp)])

   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
   -- pg :: P (String -> [(a->b), String])
   pg <*> px = P (\inp -> case parse pg inp of
                             []        -> []
                             [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   {--
      pattern matching on the result of 'parse p inp'
      result [] -> []
      otherwise, apply f on the output v which returns a monad(Parser)
      then keep parse the left string 'out'
    --}
   p >>= f = P (\inp -> case parse p inp of
                           []        -> []
                           [(v,out)] -> parse (f v) out)

-- Making choices

instance Alternative Parser where
   -- empty :: Parser a
   empty = P (\inp -> [])

   -- (<|>) :: Parser a -> Parser a -> Parser a
   -- if first parser returns empty then try the second parser
   p <|> q = P (\inp -> case parse p inp of
                           []        -> parse q inp
                           [(v,out)] -> [(v,out)])

-- Derived primitives
-- item >>= (Char -> Parser a)
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit --if the Char in item is digit

lower :: Parser Char
lower = sat isLower -- if the Char in item is lowercase letter

upper :: Parser Char
upper = sat isUpper -- if the Char in item is uppercase letter

letter :: Parser Char
letter = sat isAlpha -- if the Char in item is letter including upper case and lower case

alphanum :: Parser Char
alphanum = sat isAlphaNum -- if the Char in item is a letter or a digit

char :: Char -> Parser Char
char x = sat (== x)  -- wrap a char into Parser(Monad)

-- paser certain string into Parser.
string :: String -> Parser String
string []     = return [] -- empty Parser = P([])
string (x:xs) = do char x -- return value if not needed, only checks if parse success; if not, the entire string parsing fails
                   string xs
                   return (x:xs) -- wrap a string into Parser(Monad)

                   
-- parser for variable names
ident :: Parser String
ident = do x  <- lower
           xs <- many (alphanum <|> char '_')
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
       <|> nat

-- Handling spacing

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)
