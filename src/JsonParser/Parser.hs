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

comma :: Parser ()
comma = void $ char ','

null :: Parser JsonValue
null = token "null" *> pure NullValue

boolean :: Parser JsonValue
boolean = BooleanValue <$> (token "true"  *> pure True <|> token "false" *> pure False)
