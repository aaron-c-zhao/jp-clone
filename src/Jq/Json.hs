module Jq.Json where

data JSON =
    JNull
    | JNum {getNum:: Float} -- numbers that may be an intergeral number or a floating point number, support e notation

instance Show JSON where
  show (JNull) = "null"
  show (JNum f) = show f
