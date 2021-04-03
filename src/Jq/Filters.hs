module Jq.Filters where

import Data.List

data Filter = Identity
  | Identifier {name::String, isOptional::Bool}
  | Index {getIndex::Int, isOptional::Bool}
  | Slice {getStart::Int, getEnd::Int, isOptional::Bool}
  | Iterator {getElements::[Int], isOptional::Bool}
  | Comma {fstOp :: Filter, sndOp :: Filter}
  | Pipe {input :: Filter, output :: Filter}
  
  

instance Show Filter where
  show Identity            = "."
  show (Identifier n b)    = n ++ if b then "?" else ""
  show (Index i b)         = ".[" ++ show i ++ "]" ++ if b then "?" else ""
  show (Slice s e b)       = show s ++ ": " ++ show e ++ if b then "?" else ""
  show (Iterator es b)     = ".[" ++ intercalate "," (map show es) ++ "]" ++ if b then "?" else ""
  show (Comma f s)         = show f ++ ", " ++ show s
  show (Pipe i o)          = show i ++ ", " ++ show o

data Config = ConfigC {filters :: Filter}
