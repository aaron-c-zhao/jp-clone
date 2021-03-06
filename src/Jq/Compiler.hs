module Jq.Compiler where


import           Jq.Filters
import           Jq.Json
import qualified Data.Map as Map


type JProgram a = JSON -> Either String a

-- define function (JSON -> Either String [JSON]) for each filter 
-- inp parameter of JProgram
compile :: Filter -> JProgram [JSON]
-- Identity filter
compile (Identity) inp  = 
    return [inp]

-- Ojbect Identifier
compile (Identifier _ _) JNull        = return [JNull]
compile (Identifier n _) (JObject js) = 
    case Map.lookup n $ Map.fromList js of
        Just x -> return [x]
        Nothing -> return [JNull]
compile (Identifier _ b) json         =  
    if b then return [] else Left $ show json 
        ++ " is not an Object, it can not be indexed with identifier!"

-- Array index
compile (Index _ _) JNull         =  return [JNull]
compile (Index i _) (JArray js)   =  if i >= length js then return [JNull] else return [js !! i]
compile (Index _ b) _             =  if b then return [] else Left "Can not index non-array"

-- Array slice
compile (Slice _ _ _) JNull        = return [JNull]
compile (Slice s e _) (JArray js)  =  case drop s . take e $ js of  
        [] -> return [JArray []]
        rs -> return [JArray rs]
compile (Slice s e _) (JString str) = case drop s . take e $ str of 
    ""     -> return [JString ""]
    substr -> return [JString substr]
compile (Slice _ _ b) _             = if b then return [] else Left "Can not index non-array"

-- Iterator
compile (Iterator [] _) (JArray js)      =  return js 
compile (Iterator [] _) (JObject kvs)    =  return (map snd kvs)
compile (Iterator [] b) _                =  if b then return [] else Left "Can not iterator over non-array"
compile (Iterator _ b) (JObject _)       =  if b then return [] else Left "Can not index object with number"
compile (Iterator is b) json             =  case compile (Iterator [] b) json of
    Right [] -> return [JNull | _ <- is] 
    Right js -> return (map (\i -> if i < length js then js !! i else JNull) is)
    Left _   -> Left "Can not index non array"

-- Comma
compile (Comma f s) json =  compile f json >>= (\js -> (++) js <$> compile s json) 

-- Pipe
compile (Pipe i o) json  =  do
    is <- compile i json
    multiCompile o is

-- construct primitive values, bool, numbers, string, null
compile (JVal j) _ = Right [j]

-- construct array
compile (JArrayFilter js) json =  case compile js json of
    Left str      -> Left str
    Right [JNull] -> Right [JArray []]
    Right vs      -> Right [JArray vs]


-- construct object
compile (JKeyPair (k, v)) json = case compile k json of 
    Left str                   -> Left str
    Right [JString str]        -> compileVal str v json
    Right ks                   -> compileKeys ks v json 
    
compile (JObjectFitler (Comma l r)) json = case compile (Comma l r) json of
    Left str -> Left str
    Right js -> Right [JObject $ extractJSONs js]
        where 
            extractJSONs ((JObject kvs): objs) = kvs ++ extractJSONs objs
            extractJSONs _ = []
compile (JObjectFitler (JVal (JArray []))) _ = Right [JObject []]
compile (JObjectFitler f) json = compile f json



compileKeys :: [JSON] -> Filter -> JSON -> Either String [JSON]
compileKeys [] _ _                      = Right []
compileKeys ((JString str):keys) v json =  (++) <$> compileVal str v json <*> compileKeys keys v json
compileKeys _ _ _                       = Left "Can not use things other than string as key"


compileVal :: String -> Filter -> JSON -> Either String [JSON]
compileVal str f j = case compile f j of
        Left s    -> Left s
        Right js  -> return  $ map (\json -> JObject [(str, json)]) js


    

    
-- helper function for pipe to recursively compile it
multiCompile :: Filter -> [JSON] -> Either String [JSON]
multiCompile _ []     = Right []
multiCompile f (j:js) = compile f j >>= (\rs -> (++) rs <$> multiCompile f js)

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
