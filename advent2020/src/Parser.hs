module Parser where

import Data.Char
import Control.Applicative

-- Plagiarism from https://github.com/tsoding/haskell-json/

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      Just (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      Just (input'', f a)

instance Monad Parser where
  return = pure
  x >>= f = parserJoin (f <$> x)
    where
      parserJoin :: Parser (Parser x) -> Parser x
      parserJoin p = Parser $ \input -> do
        (input', p') <- runParser p input
        runParser p' input'

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

eof :: Parser ()
eof = Parser f
  where
    f [] = Just ([], ())
    f _  = Nothing

anyP :: Parser Char
anyP = Parser f
  where
    f (y:ys) = Just (ys, y)
    f []     = Nothing

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y:ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

stringP :: String -> Parser String
stringP = sequenceA . map charP

spanP :: (Char -> Bool) -> Parser String
spanP f =
  Parser $ \input ->
    let (token, rest) = span f input
     in Just (rest, token)

filterP :: (a -> Bool) -> Parser a -> Parser a
filterP predicate (Parser p) =
  Parser $ \input -> do
    (input', parsed) <- p input
    if predicate parsed
      then Just (input', parsed)
      else Nothing

notNull :: Parser [a] -> Parser [a]
notNull = filterP (not . null)

intP :: Parser Integer
intP = f <$> notNull (spanP isDigit)
  where f ds = read ds

ws :: Parser String
ws = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fileName parser = do
  input <- readFile fileName
  return (snd <$> runParser parser input)

evalParser :: Parser a -> String -> Maybe a
evalParser p input = snd <$> runParser p input
