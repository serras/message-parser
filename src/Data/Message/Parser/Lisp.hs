{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Data.Message.Parser.Lisp where

import Data.Message.Parser.TH
import Data.Message.Parser.Util

import Data.AttoLisp
import qualified Data.Text as T
import Language.Haskell.TH

{-
From A { B :: String, C :: String }:
* Data == show the name for the option -> (:a "b-value" "c-value")
* Record == show the name of each record before its value -> (:b "b-value" "c-value")
* Data + Record == (:a :b "b-value" :c "c-value")
-}

deriveToLispPlain :: Name -> Q [Dec]
deriveToLispPlain = deriveToLisp False False

deriveToLispData :: Name -> Q [Dec]
deriveToLispData = deriveToLisp True False

deriveToLispRecord :: Name -> Q [Dec]
deriveToLispRecord = deriveToLisp False True

deriveToLispDataRecord :: Name -> Q [Dec]
deriveToLispDataRecord = deriveToLisp True True

deriveToLisp :: Bool -> Bool -> Name -> Q [Dec]
deriveToLisp data_name record_name t = do
  -- Get list of constructors
  TyConI (DataD _ _ _ constructors _)  <-  reify t
 
  -- Make `toLisp` clause for one constructor
  let showClause (NormalC name fields) = showClause' name (length fields) getNormalCElts
      showClause (RecC name fields)    = showClause' name (length fields) $
                                           if record_name
                                              then getRecCElts (map (nameBase . (\(a,_,_) -> a)) fields)
                                              else getNormalCElts
      showClause _                     = error $ buildError "Infix or Forall not supported"
      
      showClause' name no_fields f = do
        let constructorName = nameBase name
            lispName = ':' : (toDashName constructorName)
        -- Get variables for left and right side of function definition
        (pats, vars) <- genPE no_fields
        -- Generate function clause for one constructor
        let inner = if data_name
                       then if no_fields == 0
                               then [| Symbol (T.pack lispName) |]
                               else [| mkStruct (T.pack lispName) $(f vars) |]
                       else [| List $(f vars) |]
        
        clause [conP name pats] (normalB inner) []
     
      getNormalCElts []     = [| [] |]
      getNormalCElts (v:vs) = [| (toLisp $v) : $(getNormalCElts vs) |]
      
      getRecCElts _ []          = [| [] |]
      getRecCElts (n:ns) (v:vs) = let recordName = ':' : (toDashName n)
                                  in  [| (Symbol (T.pack recordName)) : (toLisp $v) : $(getRecCElts ns vs) |]
      getRecCElts _      _      = error $ buildError "This should never happen"
 
  showbody <- mapM showClause constructors
 
  -- Generate template instance declaration and then replace
  --   type name and function body with our data
  d <- [d| instance ToLisp TheExample where
             toLisp _ = nil
       |]
  let    [InstanceD [] (AppT toLispT (ConT _TheExample)) [FunD toLispF _nil]] = d
  return [InstanceD [] (AppT toLispT (ConT t  )) [FunD toLispF showbody]]

