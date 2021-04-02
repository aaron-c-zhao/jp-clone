module Jq.Filters where

data Filter = Identity
  | Identifier {name::String, isOptional::Bool}
  | Index {getIndex::Int}
  | Slice {getStart::Int, getEnd::Int}

instance Show Filter where
  show Identity            = "."
  show (Identifier n b)    = n ++ if b then "?" else ""
  show (Index i)           = "[" ++ show i ++ "]"
  show (Slice s e)         = show s ++ ": " ++ show e


data Config = ConfigC {filters :: Filter}
