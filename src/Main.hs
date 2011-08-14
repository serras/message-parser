{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Main where

import Data.AttoLisp hiding (encode)
import Data.Aeson
import qualified Data.Text as T
import Data.Message.Parser.Json
import Data.Message.Parser.Lisp
import qualified Data.ByteString.Lazy as B

data Response = ConnectionInfo Int
              | Quitting
              | ALongThing Int T.Text

data Response2 = Connection { pid  :: T.Text, version :: Int }

$(deriveToLispPlain ''Response)
$(deriveToLispDataRecord ''Response2)
$(deriveToJsonPlain ''Response)
$(deriveToJsonDataWrapped "method" "params" ''Response2)

main :: IO ()
main = do putStrLn $ show $ toLisp (ConnectionInfo 3)
          putStrLn $ show $ toLisp (Connection "zas" 3)
          B.putStrLn $ encode $ toJSON (ConnectionInfo 3)
          B.putStrLn $ encode $ toJSON (Connection "zas" 3)

