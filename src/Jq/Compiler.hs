module Jq.Compiler where


import           Jq.Filters
import           Jq.Json
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable
import Debug.Trace


type JProgram a = JSON -> Either String a

-- define function (JSON -> Either String [JSON]) for each filter 
-- inp parameter of JProgram
compile :: Filter -> JProgram [JSON]
compile (Identity) inp = return [inp]
compile (Identifier n _) (JObject js) = case Map.lookup n $ Map.fromList js of
    Just x -> return [x]
    Nothing -> return [JNull]
compile (Identifier _ b) json =  if b then return [JNull] else Left $ show json ++ " is not an Object, it can not be indexed with identifier!"


run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
