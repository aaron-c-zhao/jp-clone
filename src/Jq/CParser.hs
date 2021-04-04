module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
import Jq.JParser 
import Jq.Json
import Debug.Trace


parseIdentity :: Parser Filter
parseIdentity = do
  _ <- token . char $ '.'
  return Identity

parseSimpleIdentifier :: Parser String
parseSimpleIdentifier =  do
  _   <- token . char $ '.'
  parseSimpleString

parseIdentifierGeneric::Parser String 
parseIdentifierGeneric = do _  <- token . char $ '.'
                            _  <- string "[\""
                            i  <- jString 
                            _  <- string "]"
                            return i
                        <|>
                            do 
                              _ <- token . char $ '.'     
                              _ <- char '\"'
                              jString 

parseIdentifier :: Parser Filter
parseIdentifier = do 
  iden <- parseSimpleIdentifier <|> parseIdentifierGeneric
  Identifier iden <$> parseOptional


parseArrayIndex :: Parser Int
parseArrayIndex = do
  _ <- token. char $ '.' 
  _ <- char '['
  index <- integer 
  _ <- char ']'
  return index
  

parseArraySlice :: Parser (Int, Int)
parseArraySlice = do 
  _     <- token . string $ ".["
  start <- integer 
  _     <- char ':'
  end   <- integer
  _     <- char ']'
  return (start, end)


parseIterator :: Parser [Int] 
parseIterator = do
  _  <- token . string $ ".["
  es <- many parseCommaSeperatedNum
  _  <- char ']'
  return es


parseArrayIndexOptional :: Parser Filter
parseArrayIndexOptional = do
  index <- parseArrayIndex
  Index index <$> parseOptional

parseArraySliceOptional :: Parser Filter
parseArraySliceOptional = do
  (s, e) <- parseArraySlice
  Slice s e <$> parseOptional

parseIteratorOptional :: Parser Filter
parseIteratorOptional = do
  is <- parseIterator
  Iterator is <$> parseOptional

parseCommaSeperatedNum :: Parser Int
parseCommaSeperatedNum = do i <- integer
                            _ <- char ','
                            return i 
                         <|>
                            integer 

parsePrimitive :: Parser Filter
parsePrimitive = parseIdentifier
  <|> parseIteratorOptional
  <|> parseArraySliceOptional
  <|> parseArrayIndexOptional
  <|> parseIdentity
  <|> parseJVal
  -- <|> parseCObject
  <|> parseCArray


parseFilter :: Parser Filter
parseFilter = parsePipe 

-- Grammar
-- pipe        ::= comma | pipe ; comma
-- comma       ::= parenthesis , comma ; parenthesis
-- parenthesis ::= () ; primitive 

parseComma :: Parser Filter
parseComma = do p <- parseParenthesis
                do  _ <- symbol ","
                    Comma p <$> parseComma
                  <|> return p
                  
parsePipe :: Parser Filter
parsePipe = do c <- parseComma
               do  _ <- symbol "|"
                   Pipe c <$> parsePipe
                 <|> Pipe c <$> parsePipe 
                 <|> return c

parseParenthesis :: Parser Filter
parseParenthesis = do _ <- symbol "("
                      p <- parsePipe
                      _ <- symbol ")"
                      return p 
                    <|> parsePrimitive

--------------------------------- Constructors---------------------------------

parseJVal :: Parser Filter
parseJVal = do 
  JVal <$> (parseJNull <|> parseJBool <|> parseJFloat <|> parseJInt <|> parseJStr)


-- parseCObject :: Parser Filter
-- parseCObject = do _   <- symbol "{"
--                   do kv <- parseCKeyPair 
--                      _  <- symbol ","
--                      return  
--                     <|> kv <- parseCKeyPair


parseCArray :: Parser Filter
parseCArray = do _  <- symbol "["
                 es <- parsePipe
                 _  <- symbol "]"
                 return $ JArrayFilter es
               <|> 
                 do _ <- symbol "["
                    _ <- symbol "]"
                    return $ JArrayFilter $ JVal JNull 
                
               

-- parseCKeyPair :: Parser Filter 
-- parseCKeyPair = do 
--   key <- parseFilter <|> (JVal <$> (JString <$> parseSimpleString))
--   _   <- symbol ":"
--   v   <- parseFilter
--   return $ JKeyValPair (key, v) 

parseSimpleString :: Parser String
parseSimpleString = do 
  f <- letter
  s <- many $ alphanum <|> char '_'
  return $ f : s


parseConfig :: [String] -> Either String Config
parseConfig s = case s of 
  [] -> Left "No filters provided"
  h : _ -> -- only parse one filter with this case if there's more than one then an error is thrown
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e


parseOptional :: Parser Bool
parseOptional = do _ <- char '?'
                   return True 
                <|> 
                   return False