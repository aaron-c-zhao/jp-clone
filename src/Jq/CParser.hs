module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
import Jq.JParser 
import Debug.Trace


parseIdentity :: Parser Filter
parseIdentity = do
  _ <- token . char $ '.'
  return Identity

parseSimpleIdentifier :: Parser String
parseSimpleIdentifier =  do
  _           <- token . char $ '.'
  first_char  <- letter 
  rest        <- many $ alphanum <|> char '_'
  return (first_char : rest)

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


parseFilter :: Parser Filter
parseFilter = parsePipe 
  <|> parseComma
  <|> parsePrimitive


parseComma :: Parser Filter
parseComma = do f <- parsePrimitive 
                _ <- token . char $ ','
                Comma f <$> parseFilter
                  
parsePipe :: Parser Filter
parsePipe = do p <- parsePrimitive
               _ <- token . char $ '|'
               Pipe p <$> parseFilter 
            <|>
               do p <- parsePrimitive
                  Pipe p <$> parseFilter
               
               

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