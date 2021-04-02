module Jq.Compiler where


import           Jq.Filters
import           Jq.Json
import Data.Map (Map)
import qualified Data.Map as Map


type JProgram a = JSON -> Either String a

-- define function (JSON -> Either String [JSON]) for each filter 
-- inp parameter of JProgram
compile :: Filter -> JProgram [JSON]
-- Identity filter
compile (Identity) inp  = 
    return [inp]

-- Ojbect Identifier
compile (Identifier n _) (JObject js) = 
    case Map.lookup n $ Map.fromList js of
        Just x -> return [x]
        Nothing -> return [JNull]
compile (Identifier _ b) json =  
    if b then return [JNull] else Left $ show json 
        ++ " is not an Object, it can not be indexed with identifier!"

-- Array index
compile (Index i _) (JArray js) =  if i >= length js then return [JNull] else return [js !! i]
compile (Index _ b) _           =  if b then return [] else Left "Can not index non-array"

-- Array slice
compile (Slice s e _) (JArray js) =  
    case drop s . take e $ js of  
        [] -> return [JArray []]
        rs -> return rs
compile (Slice _ _ b) _           = if b then return [] else Left "Can not index non-array"

-- Iterator
compile (Iterator [] _) (JArray js)      =  return js 
compile (Iterator [] _) (JObject kvs)    =  return (map snd  kvs)
compile (Iterator [] b) _                =  if b then return [] else Left "Can not iterator over non-array"
compile (Iterator is b) json             =  case compile (Iterator [] b) json of
    Right [] -> return [JNull | _ <- is] 
    Right js -> return (map (\i -> if i < length js then js !! i else JNull) is)
    Left _   -> Left "Can not index non array"


run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
