module Jq.Json where
import Data.Char

data JSON =
    JNull
    | JFloat {getFloat:: Float} 
    | JInt {getInt:: Int} 
    | JBool {getBool::Bool}
    | JString {getString::String }
    | JArray {getArray:: [JSON]}
    | JObject {getObject:: (JSON, JSON)} -- key could only be JString 

instance Show JSON where
  show JNull = "null"
  show (JFloat f) = show f
  show (JInt i) = show i
  show (JBool b) = lowerFirst $ show b -- keep the output consistant with 'jq' i.e. 'true' 'false'
  show (JString s) = s
  show (JArray xs) = showJArray 0 xs 
  show (JObject obj) = showJObject 0 obj


lowerFirst :: String -> String
lowerFirst [] = []
lowerFirst (x:xs) = toLower x : xs


indent :: Int -> String
indent n = [' '| _ <- [1..n]]


showJArray :: Int -> [JSON] -> String
showJArray n xs = indent n ++ "[" ++ showContent (n + 2) xs ++ "\n" ++ indent n ++ "]"

showElement :: Int -> JSON -> String
showElement n x = case x of
  JArray as          -> showJArray n as
  JObject (key, val) -> showJObject n (key, val)
  _                  -> indent n  ++ show x

showContent :: Int -> [JSON] -> String
showContent _ [] = "" 
showContent n [x] =  "\n" ++ showElement n x
showContent n (x:xs) = "\n" ++  showElement n x ++ "," ++ showContent n xs


showJObject :: Int -> (JSON, JSON) -> String
showJObject n (key, val) = indent n ++ "{\n" ++ showElement (n + 2) key ++ ": " ++ showVal (n + 2) val ++ "\n" ++ indent n ++ "}"
  where 
    showVal n' val' = case val' of 
      JArray xs             -> "[" ++ showContent (n' + 2) xs ++ "\n" ++ indent n' ++ "]"
      JObject (key'', val'')  -> "{" ++ showElement (n' + 2) key'' ++  showVal (n' + 2) val'' ++ indent n' ++ "}"
      _                     -> showElement 0 val'

