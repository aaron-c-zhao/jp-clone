module Jq.JParser where

import Parsing.Parsing
import Jq.Json
import Data.Char
import Debug.Trace

parseJNull :: Parser JSON
parseJNull = do _ <- string "null"
                return JNull

parseJFloat :: Parser JSON
parseJFloat = JFloat <$> float

parseJInt :: Parser JSON 
parseJInt = JInt <$> (int <|> sint)

parseJBool :: Parser JSON
parseJBool = JBool <$> bool

parseJStr :: Parser JSON
parseJStr = do _ <- symbol "\""
               JString <$> jString 

parseJArray :: Parser JSON
parseJArray = do _  <- char '['
                 es <- many jelement 
                 e  <- many parseJSON
                 _  <- char ']'
                 return (JArray (es ++ e))


parseJObject :: Parser JSON
parseJObject = do _ <- symbol "{"
                  kvs <- many jkeypairComma -- Note since jkeypairComma does not call parseJSON, it has to handle space by itself
                  kv  <- many jkeypair
                  _ <- symbol "}"
                  return $ JObject (kvs ++ kv)
                  

parseJSON :: Parser JSON
parseJSON = token $ parseJNull 
    <|> parseJFloat 
    <|> parseJInt 
    <|> parseJBool 
    <|> parseJStr 
    <|> parseJArray 
    <|> parseJObject


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


-- boolean value
bool :: Parser Bool
bool = do b <- string "true" <|> string "false"
          case b of 
              "true" -> return True
              "false" -> return False
              _ -> empty
          
-- string 

jchar :: Parser Char 
jchar = sat (\c -> (c /= '\\') && (c /= '"'))

concatAlphNum:: Parser ([Char] -> [Char])
concatAlphNum = (:) <$> alphanum

jescape :: Parser String
jescape = do 
    c <- item
    case c of
        'u'    ->  (concatAlphNum <*> (concatAlphNum <*> (concatAlphNum <*> (concatAlphNum <*> return [])))) >>= (\x -> return [x]).todec
            where
                todec:: [Char] -> Char
                todec xs = toEnum $ todec' $ reverse xs 
                    where
                        todec' []     = 0
                        todec' (y:ys) = digitToInt y + 16 * todec' ys
        '"'    -> return "\\\""
        '\\'   -> return "\\" 
        '/'    -> return "/" 
        'b'    -> return "\\b"
        'f'    -> return "\\f"
        'n'    -> return "\\n"
        'r'    -> return "\\r"
        't'    -> return "\\t"
        _      -> empty
                 

jquote :: Parser String
jquote = (:) <$> sat (== '"') <*> return []

-- recursivly parser characters in a string until encounters:
-- \ : escape sequence
-- " : end of string
-- illegal characters
jString :: Parser String
jString = do s <- many jchar
             c <- item
             case c of 
                 '\\'   -> (++) s <$> ((++) <$> jescape <*> jString) 
                 '"'    -> return s
                 _      -> return []


jelement :: Parser JSON
jelement = do e <- parseJSON
              _ <- char ','
              return e
             
jkeypair :: Parser (String, JSON)
jkeypair = do key <- parseJStr
              _   <- symbol ":"
              val <- parseJSON
              case key of 
                  JString str -> return (str, val)
                  _           -> empty

jkeypairComma :: Parser (String, JSON)
jkeypairComma = do kv <- jkeypair
                   _ <- symbol ","
                   return kv
             
                    
                    
                    

                    




