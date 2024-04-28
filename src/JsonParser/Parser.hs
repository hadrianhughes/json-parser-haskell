module JsonParser.Parser where

import Control.Applicative
import Control.Applicative.Combinators
import Data.Functor
import JsonParser.Json

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

runParser :: Parser a -> String -> Maybe a
runParser (Parser p) input =
  case p input of
    Nothing     -> Nothing
    Just (x, _) -> Just x

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input ->
    case p input of
      Nothing          -> Nothing
      Just (x, input') -> Just (f x, input')

instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)
  (Parser p1) <*> (Parser p2) = Parser $ \input ->
    case p1 input of
      Nothing          -> Nothing
      Just (f, input') ->
        case p2 input' of
          Nothing           -> Nothing
          Just (x, input'') -> Just (f x, input'')

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input ->
    case p1 input of
      Just (x, input') -> Just (x, input')
      Nothing ->
        case p2 input of
          Nothing -> Nothing
          Just (x, input') -> Just (x, input')


satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser $ \input ->
  case input of
    (x:xs) | pred x -> Just (x, xs)
    _               -> Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

token :: String -> Parser String
token t = sequenceA $ map char t

brackets :: Parser a -> Parser a
brackets = between (char '[') (char ']')

braces :: Parser a -> Parser a
braces = between (char '{') (char '}')

quotes :: Parser a -> Parser a
quotes = between (char '"') (char '"')

dot :: Parser ()
dot = void $ char '.'

comma :: Parser ()
comma = void $ char ','

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

jsonNumber :: Parser JsonValue
jsonNumber = NumberValue <$> (float <|> int)

jsonString :: Parser JsonValue
jsonString = StringValue <$> many (satisfy (/= '"'))

jsonArray :: Parser JsonValue
jsonArray = ArrayValue <$> brackets (jsonValue `sepBy` comma)

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBoolean <|> jsonNumber <|> jsonString <|> jsonArray
