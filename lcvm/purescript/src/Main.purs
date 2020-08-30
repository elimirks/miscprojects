module Main where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative, (<|>))
import Control.Lazy (class Lazy, fix)
import Control.Plus (class Plus)
import Data.Array (snoc)
import Data.Char.Unicode (isAlphaNum)
import Data.Foldable (foldl, foldr)
import Data.List (List(..), null, span)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

stringToList :: String -> List Char
stringToList = toCharArray >>> foldr Cons Nil

fromCharList :: List Char -> String
fromCharList = fromCharArray <<< foldl snoc []

data Variable = Variable String

instance showVariable :: Show Variable where
  show (Variable s) = "#" <> s

instance eqVariable :: Eq Variable where
  eq (Variable v1) (Variable v2) = v1 == v2

data Expr
  = ExprVariable Variable
  | ExprApplication Expr Expr
  | ExprAbstraction Variable Expr

instance showExpr :: Show Expr where
  show (ExprVariable v) = show v
  show (ExprApplication e1 e2) = "(" <> show e1 <> " " <> show e2 <> ")"
  show (ExprAbstraction v e) = "\\" <> show v <> "." <> show e

instance eqExpr :: Eq Expr where
  eq (ExprVariable v1) (ExprVariable v2) =
    v1 == v2
  eq (ExprApplication a1 a2) (ExprApplication b1 b2) =
    a1 == b1 && a2 == b2
  eq (ExprAbstraction a1 a2) (ExprAbstraction b1 b2) =
    a1 == b1 && a2 == b2
  eq _ _ = false

data Parser a = Parser ((List Char) -> Maybe (Tuple (List Char) a))

runParser :: forall a. Parser a -> (List Char) -> Maybe (Tuple (List Char) a)
runParser (Parser a) = a

instance parserLazy :: Lazy (Parser a) where
  defer f = Parser $ \input -> runParser (f unit) input

instance parserFunctor :: Functor Parser where
  map f (Parser p) =
    Parser $ \input -> do
      (Tuple input' x) <- (p input)
      pure $ Tuple input' (f x)

instance parserApplicative :: Applicative Parser where
  pure x = Parser $ (\input -> Just $ (Tuple input x))

instance parserApply :: Apply Parser where
  apply (Parser p1) (Parser p2) = 
    Parser $ \input -> do
      Tuple input' f <- p1 input
      Tuple input'' a <- p2 input'
      pure $ Tuple input'' (f a)

instance parserAlt :: Alt Parser where
  alt (Parser p1) (Parser p2) =
    Parser $ (\input -> p1 input <|> p2 input)

instance parserPlus :: Plus Parser where
  empty = Parser $ \_ -> Nothing

instance parserAlternative :: Alternative Parser

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (Cons y ys)
      | y == x = Just $ Tuple ys x
      | otherwise = Nothing
    f Nil = Nothing

spanP :: (Char -> Boolean) -> Parser (List Char)
spanP f =
  Parser $ \input ->
    let {init, rest} = span f input
     in Just (Tuple rest init)

ws :: Parser (List Char)
ws = spanP (\c -> case c of
               ' ' -> true
               '\t' -> true
               '\n' -> true
               _ -> false)

notNull :: forall a. Parser (List a) -> Parser (List a)
notNull (Parser p) =
  Parser $ \input -> do
    Tuple input' xs <- p input
    if null xs
      then Nothing
      else Just $ Tuple input' xs

variableP :: Parser Variable
variableP = Variable <$> fromCharList <$> notNull (spanP isAlphaNum)

exprVariableP :: Parser Expr
exprVariableP = ExprVariable <$> variableP

exprApplicationP :: Parser Expr -> Parser Expr
exprApplicationP parser =
  charP '(' *> ws *> content <* ws <* charP ')'
    where
      content = ado
        first <- parser
        _ <- notNull ws
        second <- parser
        in ExprApplication first second

parenEaterP :: Parser Expr -> Parser Expr
parenEaterP parser = charP '(' *> ws *> parser <* ws <* charP ')'

exprAbstractionP :: Parser Expr -> Parser Expr
exprAbstractionP parser = ado
  _ <- charP '\\'
  _ <- ws
  var <- variableP
  _ <- ws
  _ <- charP '.'
  expr <- parser
  in ExprAbstraction var expr

exprP :: Parser Expr
exprP = fix allExprs
  where
    allExprs p = exprVariableP <|> exprApplicationP p <|> exprAbstractionP p

main :: Effect Unit
main = do
  log $ show $ runParser exprP (stringToList "(\\x.x \\y.y)")
