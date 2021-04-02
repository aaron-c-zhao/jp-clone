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


parseArrayIndex :: Parser Filter
parseArrayIndex = do
  _ <- token. char $ '.' 
  _ <- char '['
  index <- integer 
  _ <- char ']'
  return $ Index index
  

parseArraySlice :: Parser Filter
parseArraySlice = do 
  _     <- token . string $ ".["
  start <- integer 
  _     <- char ':'
  end   <- integer
  _     <- char ']'
  return $ Slice start end

parseFilter :: Parser Filter
parseFilter = parseIdentifier
  <|> parseArraySlice
  <|> parseArrayIndex
  <|> parseIdentity

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