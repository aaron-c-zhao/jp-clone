module Jq.Json where
import Data.Char
{--
  Data types
    Numbers: done
    String: TODO
    Boolean: done
    Array: TODO
    Object: TODO
    null: done
--}




data JSON =
    JNull
    | JFloat {getFloat:: Float} 
    | JInt {getInt:: Int} 
    | JBool {getBool::Bool}
    | JString {getString::String }

instance Show JSON where
  show JNull = "null"
  show (JFloat f) = show f
  show (JInt i) = show i
  show (JBool b) = lowerFirst $ show b -- keep the output consistant with 'jq' i.e. 'true' 'false'
  show (JString s) = s


lowerFirst :: String -> String
lowerFirst [] = []
lowerFirst (x:xs) = toLower x : xs
