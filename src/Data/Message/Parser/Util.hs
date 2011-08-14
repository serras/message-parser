module Data.Message.Parser.Util where

import Data.Char
import Data.List.Utils

buildError :: String -> String
buildError s = "MPARSER: " ++ s

toDashName :: String -> String
toDashName name = join "-" (map (map toLower) (camelCaseGroups name))
  where camelCaseGroups []     = []
        camelCaseGroups (c:cs) = let (g, rest) = span isLower cs
                                 in  (c:g):(camelCaseGroups rest)

