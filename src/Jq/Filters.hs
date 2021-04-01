module Jq.Filters where

data Filter = Identity
  | Identifier {name::String, isOptional::Bool}

instance Show Filter where
  show Identity            = "."
  show (Identifier n b)      = n ++ if b then "?" else ""


data Config = ConfigC {filters :: Filter}
