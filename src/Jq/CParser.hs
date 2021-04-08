module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
import Jq.JParser 
import Jq.Json


parseIdentity :: Parser Filter
parseIdentity = do _ <- symbol "." 
                   Pipe Identity <$> parseIteratorOptional
                  <|> 
                    do _ <- symbol ".."
                       return RecursiveDescent 
                  <|>
                    do _<- symbol "."
                       return Identity 
                 

parseSimpleIdentifier :: Parser String
parseSimpleIdentifier =  do
  _   <- symbol "." 
  parseSimpleString

parseIdentifierGeneric::Parser String 
parseIdentifierGeneric = do _  <- symbol "." 
                            _  <- string "[\""
                            i  <- jString 
                            _  <- string "]"
                            return i
                        <|>
                            do 
                              _ <- symbol "." 
                              _ <- char '\"'
                              jString 

parseIdentifier :: Parser Filter
parseIdentifier = do iden <- parseSimpleIdentifier <|> parseIdentifierGeneric
                     do b <- parseOptional
                        Pipe (Identifier iden b) <$> parseIdentifier
                        <|>
                          do 
                            b <- parseOptional
                            Pipe (Identifier iden b) <$> parseIteratorOptional
                        <|>
                           Identifier iden <$> parseOptional


parseArrayIndex :: Parser Int
parseArrayIndex = do
  _ <- symbol ".[" 
  index <- integer 
  _ <- symbol "]" 
  return index
  

parseArraySlice :: Parser (Maybe Int, Maybe Int)
parseArraySlice = do _     <- symbol ".["
                     do start <- integer 
                        _     <- symbol ":"
                        end   <- integer
                        _     <- symbol "]"
                        return (Just start, Just end)
                        <|>
                          do _   <- symbol ":"
                             end' <- integer 
                             _   <- symbol "]"
                             return (Nothing, Just end')
                            <|> 
                              do start' <- integer 
                                 _     <- symbol ":"
                                 _     <- symbol "]"
                                 return (Just start', Nothing)
                        
                      

parseIterator :: Parser [Int] 
parseIterator = do 
  _   <- symbol "["
  es  <- many parseCommaSeperatedNum
  _   <- symbol "]" 
  return es


parseArrayIndexOptional :: Parser Filter
parseArrayIndexOptional = do index <- parseArrayIndex
                             do
                                b     <- parseOptional
                                Pipe (Index index b) <$> parseIteratorOptional
                              <|>
                                Index index <$> parseOptional

parseArraySliceOptional :: Parser Filter
parseArraySliceOptional = do (s, e) <- parseArraySlice
                             do b <- parseOptional
                                Pipe (Slice s e b) <$> parseIteratorOptional
                              <|>
                                Slice s e <$> parseOptional


parseIteratorOptional :: Parser Filter
parseIteratorOptional = do
  is <- parseIterator
  Iterator is <$> parseOptional

parseCommaSeperatedNum :: Parser Int
parseCommaSeperatedNum = do i <- integer
                            _ <- symbol ","
                            return i 
                         <|>
                            integer 

parseGenericIndex :: Parser Filter
parseGenericIndex = do _ <- symbol ".["
                       f <- parseFilter
                       _ <- symbol "]"
                       GenericIndex f <$> parseOptional

parsePrimitive :: Parser Filter
parsePrimitive = parseIdentifier
  <|> parseArraySliceOptional
  <|> parseArrayIndexOptional
  <|> parseIdentity
  <|> parseJVal
  <|> parseCObject
  <|> parseCArray
  <|> parseGenericIndex


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
                 <|> return c

parseParenthesis :: Parser Filter
parseParenthesis = do _ <- symbol "("
                      p <- parsePipe
                      _ <- symbol ")"
                      return p 
                    <|> parsePrimitive

parseRecursiveDescent :: Parser Filter
parseRecursiveDescent = do _ <- symbol ".."
                           return RecursiveDescent 

--------------------------------- Constructors---------------------------------

parseJVal :: Parser Filter
parseJVal = do 
  JVal <$> (parseJNull <|> parseJBool <|> parseJFloat <|> parseJInt <|> parseJStr)


parseCObject :: Parser Filter
parseCObject = do _   <- symbol "{"
                  do kvs <- parseCKeyPairComma  
                     _   <- symbol "}"
                     return $ JObjectFitler kvs
                    <|>
                      do
                        _ <- symbol "}"
                        return $ JObjectFitler (JVal (JArray []))

parseCArray :: Parser Filter
parseCArray = do _  <- symbol "["
                 es <- parsePipe
                 _  <- symbol "]"
                 return $ JArrayFilter es
               <|> 
                 do _ <- symbol "["
                    _ <- symbol "]"
                    return $ JArrayFilter $ JVal JNull 
                
               
-- JKeyPair (Filter, Filter)
parseCKeyPair :: Parser Filter 
parseCKeyPair = do key <- parseFilter <|> (JVal <$> (JString <$> parseSimpleString))
                   _   <- symbol ":"
                   v   <- parseFilter
                   return $ JKeyPair (key, v)

-- Parser Filter
parseCKeyPairComma :: Parser Filter 
parseCKeyPairComma = do kp <- parseCKeyPair
                        _  <- symbol ","
                        Comma kp <$> parseCKeyPairComma 
                      <|>
                        do str <- parseSimpleString
                           _   <- symbol ","
                           Comma (JKeyPair (JVal $ JString str, Identifier str False)) <$> parseCKeyPairComma
                      <|>
                        parseCKeyPair
                      <|>
                        do str <- parseSimpleString
                           return $ JKeyPair (JVal $ JString str, Identifier str False)

                        

parseSimpleString :: Parser String
parseSimpleString = do f <- letter
                       s <- many $ alphanum <|> char '_'
                       return $ f : s
                      <|> 
                       do
                          _ <- symbol "\""
                          jString  





parseConfig :: [String] -> Either String Config
parseConfig s = case s of 
  [] -> Left "No filters provided"
  h : _ -> -- only parse one filter with this case if there's more than one then an error is thrown
    case parse parseFilter $ h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e


parseOptional :: Parser Bool
parseOptional = do _ <- char '?'
                   return True 
                <|> 
                   return False