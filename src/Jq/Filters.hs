module Jq.Filters where

import Data.List

data Filter = Identity
  | Identifier {name::String, isOptional::Bool}
  | Index {getIndex::Int}
  | Slice {getStart::Int, getEnd::Int}
  | Iterator {getElements::[Int]}
  

instance Show Filter where
  show Identity            = "."
  show (Identifier n b)    = n ++ if b then "?" else ""
  show (Index i)           = ".[" ++ show i ++ "]"
  show (Slice s e)         = show s ++ ": " ++ show e
  show (Iterator es)       = ".[" ++ intercalate "," (map show es) ++ "]"


data Config = ConfigC {filters :: Filter}
