module Interpreter.Parser where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative, empty, (<|>))
import Control.Lazy (class Lazy, fix)
import Control.MonadZero (guard)
import Control.Plus (class Plus)
import Data.Array (snoc)
import Data.Char.Unicode (isAlphaNum)
import Data.Either (Either(..))
import Data.Foldable (foldl, foldr)
import Data.List (List(..), many, null, span, (:))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..))

stringToList :: String -> List Char
stringToList = toCharArray >>> foldr Cons Nil

fromCharList :: List Char -> String
fromCharList = fromCharArray <<< foldl snoc []

type Variable = String

data Constant = Constant Variable Expr

data Expr
  = ExprVariable String
  | ExprApplication Expr Expr
  | ExprAbstraction Variable Expr

instance showExpr :: Show Expr where
  show (ExprVariable v) = "#" <> v
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

data Parser a = Parser ((List Char) -> Either String (Tuple (List Char) a))

runParser :: forall a. Parser a -> (List Char) -> Either String (Tuple (List Char) a)
runParser (Parser a) = a

instance parserLazy :: Lazy (Parser a) where
  defer f = Parser $ \input -> runParser (f unit) input

instance parserFunctor :: Functor Parser where
  -- map :: forall a b. (a -> b) -> f a -> f b
  map f (Parser p) =
    Parser $ \input -> do
      (Tuple input' x) <- (p input)
      pure $ Tuple input' (f x)

instance parserApplicative :: Applicative Parser where
  pure x = Parser $ (\input -> Right $ (Tuple input x))

instance parserApply :: Apply Parser where
  -- apply :: forall a b. f (a -> b) -> f a -> f b
  apply (Parser p1) (Parser p2) = 
    Parser $ \input -> do
      Tuple input' f  <- p1 input
      Tuple input'' a <- p2 input'
      pure $ Tuple input'' (f a)

instance parserAlt :: Alt Parser where
  alt (Parser p1) (Parser p2) =
    Parser $ (\input -> p1 input <|> p2 input)

instance parserPlus :: Plus Parser where
  empty = Parser $ \_ -> Left "Empty parser"

instance parserAlternative :: Alternative Parser

instance parserBind :: Bind Parser where
  bind :: forall a b. Parser a -> (a -> Parser b) -> Parser b
  bind p f = parserJoin (f <$> p)
    where
      parserJoin :: forall a. Parser (Parser a) -> Parser a
      parserJoin (Parser p) = Parser $ \input -> do
        (Tuple input' (Parser p')) <- (p input)
        (Tuple input'' x)          <- (p' input')
        pure $ Tuple input'' x

instance parserMonad :: Monad Parser

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (Cons y ys) | y == x = Right $ Tuple ys x
    f _ = Left $ "Expected character " <> show x <> " was not found"

spanP :: (Char -> Boolean) -> Parser (List Char)
spanP f =
  Parser $ \input ->
    let {init, rest} = span f input
     in Right (Tuple rest init)

ws :: Parser (List Char)
ws = spanP (\c -> case c of
               ' '  -> true
               '\t' -> true
               '\n' -> true
               _    -> false)

notNull :: forall a. Parser (List a) -> Parser (List a)
notNull (Parser p) =
  Parser $ \input -> do
    Tuple input' xs <- p input
    if null xs
      then Left "Expected multiple values, got nothing"
      else Right $ Tuple input' xs

variableP :: Parser Variable
variableP = fromCharList <$> notNull (spanP isAlphaNumButNotLambda)
  where
    isAlphaNumButNotLambda 'λ' = false
    isAlphaNumButNotLambda c   = isAlphaNum c

exprVariableP :: Parser Expr
exprVariableP = ExprVariable <$> variableP

sepBy :: forall a b. Parser a -> Parser b -> Parser (List b)
sepBy sep parser = Cons <$> parser <*> many (sep *> parser) <|> pure Nil

-- (a c)
exprApplicationP :: Parser Expr -> Parser Expr
exprApplicationP parser =
  charP '(' *> ws *> (exprsToApplications =<< components) <* ws <* charP ')'
    where
      components :: Parser (List Expr)
      components = sepBy (notNull ws) parser
      
      exprsToApplications :: List Expr -> Parser Expr
      exprsToApplications (first:second:rest) =
        pure $ foldl ExprApplication (ExprApplication first second) rest
      exprsToApplications _ = empty

exprAbstractionP :: Parser Expr -> Parser Expr
exprAbstractionP parser = ado
  _    <- spanP (\c -> c == '\\' || c == 'λ')
  _    <- ws
  var  <- variableP
  _    <- ws
  _    <- charP '.'
  expr <- parser
  in ExprAbstraction var expr

exprP :: Parser Expr
exprP = fix fullParser
  where
    fullParser p = exprParser p <|> parenEater p
    exprParser p = exprVariableP <|> exprApplicationP p <|> exprAbstractionP p
    parenEater p = charP '(' *> ws *> p <* ws <* charP ')'

constantP :: Parser Constant
constantP = ado
  var  <- variableP
  _    <- ws
  _    <- charP ':'
  _    <- charP '='
  _    <- ws
  expr <- exprP
  in Constant var expr

programP :: Parser Expr
programP = ado
  _           <- ws
  constants   <- sepBy (notNull ws) constantP
  _           <- ws
  mainExpr    <- exprP
  _           <- ws
  in foldr addConstant mainExpr constants
    where
      addConstant :: Constant -> Expr -> Expr
      addConstant (Constant var expr) acc =
        ExprApplication (ExprAbstraction var acc) expr

-- Strips anything between "#" and "\n"
stripComments :: List Char -> List Char
stripComments = stripNonComment
  where
    stripNonComment Nil    = Nil
    stripNonComment (x:xs) =
      if x == '#'
         then stripInComment xs
         else x : (stripNonComment xs)

    stripInComment Nil    = Nil
    stripInComment (x:xs) =
      if x == '\n'
         then stripNonComment xs
         else stripInComment xs

generateAST :: String -> Either String Expr
generateAST input = do
  (Tuple rest ast) <- runParser programP (stripComments $ stringToList input)
  _ <- if null rest
         then pure unit
         else Left $ "Garbage at end of input: " <> fromCharList rest
  pure ast
