module JsonParser.Parser where

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
