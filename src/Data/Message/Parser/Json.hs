{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Data.Message.Parser.Json where

import Data.Message.Parser.TH
import Data.Message.Parser.Util

import Data.Aeson
import qualified Data.Text as T
import Data.Vector (fromList)
import Language.Haskell.TH

{-
From A { B :: String, C :: String }:
* Wrapped == wraps the values inside another element -> { "type" : "a", "params" : { "b" : ... } }
* Data key == adds a new pair to show the data type -> { "key" : "a", ... }
* NoRecord == show the values in an array instead of an object -> [ "b-value", "c-value" ]
-}

deriveToJsonPlain :: Name -> Q [Dec]
deriveToJsonPlain = deriveToJson Nothing True Nothing

deriveToJsonWrapped :: String -> Name -> Q [Dec]
deriveToJsonWrapped params = deriveToJson Nothing True (Just params)

deriveToJsonNoRecord :: Name -> Q [Dec]
deriveToJsonNoRecord = deriveToJson Nothing False Nothing

deriveToJsonData :: String -> Name -> Q [Dec]
deriveToJsonData key = deriveToJson (Just key) True Nothing

deriveToJsonDataWrapped :: String -> String -> Name -> Q [Dec]
deriveToJsonDataWrapped key params = deriveToJson (Just key) True (Just params)

deriveToJson :: Maybe String -> Bool -> Maybe String -> Name -> Q [Dec]
deriveToJson data_key record_name wrap_params t = do
  -- Get list of constructors
  TyConI (DataD _ _ _ constructors _)  <-  reify t
 
  -- Make `toLisp` clause for one constructor
  let showClause (NormalC name fields) = showClauseNoRecord name (length fields)
      showClause (RecC name fields)    = if record_name
                                            then showClauseWithRecord name fields
                                            else showClauseNoRecord name (length fields)
      showClause _                     = error $ buildError "Infix or Forall not supported"
      
      showClauseNoRecord name no_fields = do
        let constructorName = nameBase name
            jsonName = toDashName constructorName
        -- Get variables for left and right side of function definition
        (pats, vars) <- genPE no_fields
        -- Generate function clause for one constructor
        let inner = case data_key of
                      Nothing -> case wrap_params of
                                   Nothing -> [| Array (fromList $(getArrayElts vars)) |]
                                   Just pn -> [| object [ T.pack pn .= Array (fromList $(getArrayElts vars)) ] |]
                      Just dn -> case wrap_params of
                                   Nothing ->  error $ buildError "Can't include data name in an not wrapped array"
                                   Just pn -> [| object [ T.pack dn .= T.pack jsonName
                                                        , T.pack pn .= Array (fromList $(getArrayElts vars)) ] |]
        
        clause [conP name pats] (normalB inner) []
      
      showClauseWithRecord name fields = do
        let constructorName = nameBase name
            jsonName = toDashName constructorName
            fieldNames = (map (nameBase . (\(a,_,_) -> a)) fields)
        -- Get variables for left and right side of function definition
        (pats, vars) <- genPE (length fields)
        -- Generate function clause for one constructor
        let inner = case data_key of
                      Nothing    -> case wrap_params of
                                     Nothing -> [| object $(getMapElts fieldNames vars) |]
                                     Just pn -> [| object [ T.pack pn .= object $(getMapElts fieldNames vars) ] |]
                      Just dname -> case wrap_params of
                                     Nothing -> [| object ((T.pack dname .= T.pack jsonName) : $(getMapElts fieldNames vars)) |]
                                     Just pn -> [| object [ T.pack dname .= T.pack jsonName
                                                          , T.pack pn .= object $(getMapElts fieldNames vars) ] |]
        
        clause [conP name pats] (normalB inner) []
      
      getArrayElts []     = [| [] |]
      getArrayElts (v:vs) = [| (toJSON $v) : $(getArrayElts vs) |]
      
      getMapElts _ []          = [| [] |]
      getMapElts (n:ns) (v:vs) = let recordName = toDashName n
                                 in  [| ((T.pack recordName) .= (toJSON $v)) : $(getMapElts ns vs) |]
      getMapElts _      _      = error $ buildError "This should never happen"
 
  showbody <- mapM showClause constructors
 
  -- Generate template instance declaration and then replace
  --   type name and function body with our data
  d <- [d| instance ToJSON TheExample where
             toJSON _ = Null
       |]
  let    [InstanceD [] (AppT toLispT (ConT _TheExample)) [FunD toLispF _nil]] = d
  return [InstanceD [] (AppT toLispT (ConT t  )) [FunD toLispF showbody]]

