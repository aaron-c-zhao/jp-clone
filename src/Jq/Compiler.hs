module Jq.Compiler where

import           Jq.Filters
import           Jq.Json


type JProgram a = JSON -> Either String a

-- define function (JSON -> Either String [JSON]) for each filter 
-- inp parameter of JProgram
compile :: Filter -> JProgram [JSON]
compile (Identity) inp = return [inp]

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
