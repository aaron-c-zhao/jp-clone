module Jq.Json where

data JSON =
    JNull
    | JFloat {getFloat:: Float} 
    | JInt {getInt:: Int} 

instance Show JSON where
  show (JNull) = "null"
  show (JFloat f) = show f
  show (JInt i) = show i
