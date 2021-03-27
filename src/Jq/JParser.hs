module Jq.JParser where

import Parsing.Parsing
import Jq.Json

parseJNull :: Parser JSON
parseJNull = do _ <- string "null"
                return JNull

parseJFloat :: Parser JSON
parseJFloat = JFloat <$> float

parseJInt :: Parser JSON 
parseJInt = JInt <$> int

parseJBool :: Parser JSON
parseJBool = JBool <$> bool

-- TODO: escape character
parseJStr :: Parser JSON
parseJStr = do _ <- char '"'
               s <- many jchar
               _ <- char '"'
               return (JString ("\"" ++ s ++ "\""))

             

parseJSON :: Parser JSON
parseJSON = token $ parseJNull <|> parseJFloat <|> parseJInt <|> parseJBool <|> parseJStr



-- positive float number
pfloat :: Parser Float 
pfloat = do xs <- some digit
            _ <- char '.'
            ys <- some digit
            return (read (xs ++ "." ++ ys))

-- negative float number
nfloat :: Parser Float
nfloat = do _ <- char '-'
            f <- pfloat
            return (-f)
        <|>
            pfloat

-- float number 
float :: Parser Float
float = do f <- nfloat
           _ <- char 'E' <|> char 'e'
           e <- int
           return (f * (10 ^ e))
        <|>
            nfloat 

-- scientific notion of integer
sint :: Parser Int
sint = do i <- int
          _ <- char 'E' <|> char 'e'
          e <- int
          return (i * (10 ^ e))


bool :: Parser Bool
bool = do b <- string "true" <|> string "false"
          case b of 
              "true" -> return True
              "false" -> return False
              _ -> empty
          
isNotQuote :: Char -> Bool 
isNotQuote c = c /= '"'

jchar :: Parser Char 
jchar = sat isNotQuote 