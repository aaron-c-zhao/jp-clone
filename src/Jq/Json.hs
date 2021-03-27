module Jq.Json where
import Data.Char
{--
  Data types
    Numbers: done
    String: TODO
    Boolean: TODO
    Array: TODO
    Object: TODO
    null: done
--}




data JSON =
    JNull
    | JFloat {getFloat:: Float} 
    | JInt {getInt:: Int} 
    | JBool {getBool::Bool}

instance Show JSON where
  show (JNull) = "null"
  show (JFloat f) = show f
  show (JInt i) = show i
  show (JBool b) = lowerFirst $ show b -- keep the output consistant with 'jq' i.e. 'true' 'false'


lowerFirst :: String -> String
lowerFirst [] = []
lowerFirst (x:xs) = toLower x : xs
