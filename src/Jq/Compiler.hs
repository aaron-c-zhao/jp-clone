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
compile (Index i _) (JArray js)   =  
    let j = convertNegativeIndex (length js) i in 
        if j >= length js || j < 0 then return [JNull] 
            else return [js !! j]
compile (Index _ b) _             =  if b then return [] else Left "Can not index non-array"


-- Generic index
compile (GenericIndex f b) (JObject kvs) = case compile f (JObject kvs) of
    Left str -> Left str
    Right js -> extractIdentifiers js 
        where 
            extractIdentifiers []                   = Right [] 
            extractIdentifiers ((JString str) : strs) = do fst'  <- compile (Identifier str b) (JObject kvs)  
                                                           rest <- extractIdentifiers strs
                                                           return $ fst' ++ rest
            extractIdentifiers _                    = Left "Can not index object with things other than string"

compile (GenericIndex f b) (JArray js) = case compile f (JArray js) of 
    Left str -> Left str
    Right ns -> extractIndices ns
        where
            extractIndices :: [JSON] -> Either String [JSON]
            extractIndices []              = Right []
            extractIndices ((JInt i): is)  = do fst'  <- compile (Index i b) (JArray js)  
                                                rest <- extractIndices is
                                                return $ fst' ++ rest
            extractIndices _               = Left "Can not index array with things other than integer"

compile (GenericIndex _ b) _ = if b then return [] else Left "Can not index whatever it is"


-- Array slice
compile (Slice _ _ _) JNull        = return [JNull]
compile (Slice s e _) (JArray js)  =  case drop s'' . take e'' $ js of  
        [] -> return [JArray []]
        rs -> return [JArray rs]
        where
            l = length js
            s' = case s of 
                Nothing -> 0 
                Just n  -> convertNegativeIndex l n 
            e' = case e of  
                Nothing -> l
                Just n  -> convertNegativeIndex l n
            s'' = if s' >= 0 then s' else 0
            e'' = if e' >= 0 then e' else 0
compile (Slice s e _) (JString str) = case drop s'' . take e'' $ str of 
    ""     -> return [JString ""]
    substr -> return [JString substr]
    where
        l = length str
        s' = case s of 
            Nothing -> 0 
            Just n  -> convertNegativeIndex l n 
        e' = case e of  
            Nothing -> l
            Just n  -> convertNegativeIndex l n
        s'' = if s' >= 0 then s' else 0
        e'' = if e' >= 0 then e' else 0
compile (Slice _ _ b) _             = if b then return [] else Left "Can not index non-array"

-- Iterator
compile (Iterator [] _) (JArray js)      =  return js 
compile (Iterator [] _) (JObject kvs)    =  return (map snd kvs)
compile (Iterator [] b) _                =  if b then return [] else Left "Can not iterator over non-array"
compile (Iterator _ b) (JObject _)       =  if b then return [] else Left "Can not index object with number"
compile (Iterator is b) json             =  case compile (Iterator [] b) json of
    Right [] -> return [JNull | _ <- is] 
    Right js -> return (map (\i -> let i' = convertNegativeIndex (length js) i in 
        if i' < length js && i' >= 0 then js !! i' else JNull) is)
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

compile RecursiveDescent json = case json of
    JArray js   -> do   i  <- compile Identity json 
                        es <- mapM (compile RecursiveDescent) js
                        return $ i ++ concat es
    JObject kvs -> do   i <- compile Identity json 
                        ss <- mapM (compile RecursiveDescent) $ map snd kvs
                        return $ i ++ concat ss
    _           -> do   compile Identity json


compileKeys :: [JSON] -> Filter -> JSON -> Either String [JSON]
compileKeys [] _ _                      = Right []
compileKeys ((JString str):keys) v json =  (++) <$> compileVal str v json <*> compileKeys keys v json
compileKeys _ _ _                       = Left "Can not use things other than string as key"


compileVal :: String -> Filter -> JSON -> Either String [JSON]
compileVal str f j = case compile f j of
        Left s    -> Left s
        Right js  -> return  $ map (\json -> JObject [(str, json)]) js


convertNegativeIndex :: Int -> Int -> Int 
convertNegativeIndex l i = if i >= 0 then i else l + i 
    

    
-- helper function for pipe to recursively compile it
multiCompile :: Filter -> [JSON] -> Either String [JSON]
multiCompile _ []     = Right []
multiCompile f (j:js) = compile f j >>= (\rs -> (++) rs <$> multiCompile f js)

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
