{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Data.Message.Parser.TH where

import Control.Monad
import Language.Haskell.TH

-- From https://doc-08-7s-docs.googleusercontent.com/docs/securesc/kt0q789038pnepb0qdmkq7ju1gcm87ag/gcar51m85lo4kfv8oph8g966vfcnitmf/1313311500000/10578434965449041783/03033175179274617222/0B4BgTwf_ng_TM2MxZjJjZjctMTQ0OS00YzcwLWE5N2QtMDI0YzE4NGUwZDM3?nonce=039f9u018m3ji&user=03033175179274617222&hash=04tu5tlbo3ee84bcl6m3ejqgdf5u6li4#_Example:_deriveShow

-- Generate n unique variables and return them in form of patterns and expressions
genPE :: Int -> Q ([PatQ], [ExpQ])
genPE n = do
  ids <- replicateM n (newName "x")
  return (map varP ids, map varE ids)
  
data TheExample = TheExample

