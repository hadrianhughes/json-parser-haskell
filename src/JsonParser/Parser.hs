module JsonParser.Parser where

import           Control.Applicative
import           Control.Applicative.Combinators
import           Data.Functor
import qualified Data.Map as M
import           JsonParser.Json
import           JsonParser.Utils

data ParseError = EndOfInput
                | Unexpected Char
                | Empty
                deriving Show

newtype Parser a = Parser { parse :: String -> Either ParseError (a, String) }

runParser :: Parser a -> String -> Either ParseError a
runParser (Parser p) input =
  case p input of
    Left e       -> Left e
    Right (x, _) -> Right x

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input ->
    case p input of
      Left e            -> Left e
      Right (x, input') -> Right (f x, input')

instance Applicative Parser where
  pure x = Parser $ \input -> Right (x, input)
  (Parser p1) <*> (Parser p2) = Parser $ \input ->
    case p1 input of
      Left e            -> Left e
      Right (f, input') ->
        case p2 input' of
          Left e             -> Left e
          Right (x, input'') -> Right (f x, input'')

instance Alternative Parser where
  empty = Parser $ \_ -> Left Empty
  (Parser p1) <|> (Parser p2) = Parser $ \input ->
    case p1 input of
      Right (x, input') -> Right (x, input')
      Left _ ->
        case p2 input of
          Left e            -> Left e
          Right (x, input') -> Right (x, input')


satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser $ \input ->
  case input of
    (x:xs)
      | pred x    -> Right (x, xs)
      | otherwise -> Left (Unexpected x)
    []            -> Left EndOfInput

sc :: Parser ()
sc = void $ many $ satisfy (`elem` " \t")

sc' :: Parser ()
sc' = void $ many $ satisfy (`elem` " \t\n")

char :: Char -> Parser Char
char c = satisfy (== c)

token :: String -> Parser String
token t = sc *> sequenceA (map char t)

brackets :: Parser a -> Parser a
brackets = between (char '[' <* sc') (sc' *> char ']')

braces :: Parser a -> Parser a
braces = between (char '{' <* sc') (sc' *> char '}')

quotes :: Parser a -> Parser a
quotes = between (char '"') (char '"')

dot :: Parser ()
dot = void $ char '.'

comma :: Parser ()
comma = void $ char ','

colon :: Parser ()
colon = void $ char ':'

jsonNull :: Parser JsonValue
jsonNull = token "null" *> pure NullValue

jsonBoolean :: Parser JsonValue
jsonBoolean = BooleanValue <$> (token "true"  *> pure True <|> token "false" *> pure False)

digit :: Parser Char
digit = satisfy (`elem` "0123456789")

int :: Parser NumLit
int = IntLit . read <$> some digit

float :: Parser NumLit
float = FloatLit <$> (constructDouble <$> some digit <*> (dot *> some digit))
  where
    constructDouble :: String -> String -> Double
    constructDouble l r = read $ l <> "." <> r

string :: Parser String
string = quotes (many $ satisfy (/= '"'))

jsonNumber :: Parser JsonValue
jsonNumber = NumberValue <$> (float <|> int)

jsonString :: Parser JsonValue
jsonString = StringValue <$> string

jsonArray :: Parser JsonValue
jsonArray = ArrayValue <$> brackets (jsonValue `sepBy` (comma *> sc'))

jsonObject :: Parser JsonValue
jsonObject = ObjectValue <$> (M.fromList <$> braces (kvPair `sepBy` (comma *> sc')))
  where
    kvPair :: Parser (String, JsonValue)
    kvPair = pair <$> string <*> (sc *> colon *> sc *> jsonValue)

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBoolean <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

jsonRoot :: Parser JsonValue
jsonRoot = jsonObject <|> jsonArray
