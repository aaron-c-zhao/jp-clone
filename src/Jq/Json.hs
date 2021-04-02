module Jq.Json where
import Data.Char

data JSON =
    JNull
    | JFloat {getFloat:: Float} 
    | JInt {getInt:: Int} 
    | JBool {getBool::Bool}
    | JString {getString::String}
    | JArray {getArray:: [JSON]}
    | JObject {getObject::[(String,JSON)]}

instance Show JSON where
  show JNull = "null"
  show (JFloat f) = show f
  show (JInt i) = show i
  show (JBool b) = lowerFirst $ show b -- keep the output consistant with 'jq' i.e. 'true' 'false'
  show (JString s) = s
  show (JArray xs) = showJArray 0 xs 
  show (JObject xs) = showJObject 0 xs

-- instance Eq JSON where
--   (JFloat f) == (JFloat f')                         = f == f'
--   (JInt i) == (JInt i')                             = i == i'
--   JNull == JNull                                    = True
--   (JBool b) == (JBool b')                           = b == b'
--   (JString s) == (JString s')                       = s == s'
--   (JArray []) == (JArray [])                        = True
--   (JArray []) == (JArray _)                         = False
--   (JArray _)  == (JArray[])                         = False
--   (JArray (x:xs)) == (JArray (x':xs'))              = (x == x') && (JArray xs) == (JArray xs')
--   (JKeyPair (key, val)) == (JKeyPair (key', val'))  = key == key' && (val == val')

lowerFirst :: String -> String
lowerFirst [] = []
lowerFirst (x:xs) = toLower x : xs


indent :: Int -> String
indent n = [' '| _ <- [1..n]]

showJArray :: Int -> [JSON] -> String
showJArray n [] = indent n ++ "[" ++ "]"
showJArray n xs = indent n ++ "[" ++ showContent (n + 2) xs ++ "\n" ++ indent n ++ "]"

showElement :: Int -> JSON -> String
showElement n x = case x of
  JArray as          -> showJArray n as
  JObject xs         -> showJObject n xs
  _                  -> indent n  ++ show x

showContent :: Int -> [JSON] -> String
showContent _ [] = "" 
showContent n [x] =  "\n" ++ showElement n x
showContent n (x:xs) = "\n" ++  showElement n x ++ "," ++ showContent n xs


showJObject :: Int -> [(String, JSON)] -> String
showJObject n xs = indent n ++ "{" ++ showKeyPairs n xs ++ "\n" ++ indent n ++ "}" 

showKeyPairs :: Int -> [(String, JSON)] -> String
showKeyPairs _ []     = ""
showKeyPairs n [x]    = showKeyPair n x
showKeyPairs n (x:xs) = showKeyPair n x ++ showKeyPairs n xs

showKeyPair :: Int -> (String, JSON) -> String
showKeyPair n (key, val) = "\n" ++ indent (n + 2) ++ show key ++ ": " ++ showVal (n + 2) val
  where 
    showVal n' val' = case val' of 
      JArray xs   -> "[" ++ showContent (n' + 2) xs ++ "\n" ++ indent n' ++ "]"
      JObject xs  -> "{\n" ++ showKeyPairs (n' + 2) xs ++ "\n" ++ indent n' ++ "}" 
      _           -> showElement 0 val'

