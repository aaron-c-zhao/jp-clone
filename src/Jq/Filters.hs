module Jq.Filters where

import Data.List
import Jq.Json

data Filter = Identity
  | Identifier {name::String, isOptional::Bool}
  | Index {getIndex::Int, isOptional::Bool}
  | GenericIndex {getGenericIndex::Filter, isOptional::Bool}
  | Slice {getStart::Int, getEnd::Int, isOptional::Bool}
  | Iterator {getElements::[Int], isOptional::Bool}
  | Comma {fstOp :: Filter, sndOp :: Filter}
  | Pipe {input :: Filter, output :: Filter}
  | JVal {val :: JSON}
  | JKeyPair {getKeyPair:: (Filter, Filter)}
  | JObjectFitler {getContent:: Filter}
  | JArrayFilter {arrFoilter:: Filter}
  | RecursiveDescent
  

instance Show Filter where
  show Identity             = "."
  show (Identifier n b)     = n ++ if b then "?" else ""
  show (Index i b)          = ".[" ++ show i ++ "]" ++ if b then "?" else ""
  show (Slice s e b)        = show s ++ ": " ++ show e ++ if b then "?" else ""
  show (Iterator es b)      = ".[" ++ intercalate "," (map show es) ++ "]" ++ if b then "?" else ""
  show (Comma f s)          = show f ++ ", " ++ show s
  show (Pipe i o)           = show i ++ ", " ++ show o
  show (JVal j)             = show j
  show (JObjectFitler f)    = "{" ++ show f ++ "}"
  show (JArrayFilter f)     = "[" ++ show f ++"]"
  show (JKeyPair (k, v))    = "(" ++ show k ++ show v ++ ")"
  show (GenericIndex f b)   = ".[" ++ show f ++ if b then "?" else "" ++ "]"
  show RecursiveDescent     = ".." 

data Config = ConfigC {filters :: Filter}
