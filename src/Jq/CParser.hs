module Jq.CParser where

import Parsing.Parsing
import Jq.Filters

parseIdentity :: Parser Filter
parseIdentity = do
  _ <- token . char $ '.'
  return Identity



parseFilter :: Parser Filter
parseFilter = parseIdentity

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ -> -- only parse one filter with this case if there's more than one then an error is thrown
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
