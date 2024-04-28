module JsonParser.Json where

import qualified Data.Map as M

data NumLit = IntLit Int | FloatLit Double deriving (Show, Eq)

data JsonValue = ObjectValue  (M.Map String JsonValue)
               | ArrayValue   [JsonValue]
               | StringValue  String
               | NumberValue  NumLit
               | BooleanValue Bool
               | NullValue
               deriving (Show, Eq)
