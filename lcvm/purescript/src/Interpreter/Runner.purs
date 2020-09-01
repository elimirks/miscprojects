module Interpreter.Runner where

import Prelude

import Data.Either (Either(..))
import Interpreter.Parser (Expr(..), generateAST)

prettyPrint :: Expr -> String
prettyPrint (ExprVariable name) = name
prettyPrint (ExprApplication target param) =
  "(" <> prettyPrint target <> " " <> prettyPrint param <> ")"
prettyPrint (ExprAbstraction arg body) =
  "λ" <> arg <> "." <> prettyPrint body

populateVar :: String -> Expr -> Expr -> Expr
populateVar varName varExpr expr@(ExprVariable name) =
  if varName == name
     then varExpr
     else expr

populateVar varName varExpr expr@(ExprAbstraction absVar absBody) =
  if absVar == varName
     then expr
     else ExprAbstraction absVar (populateVar varName varExpr absBody)

populateVar varName varExpr (ExprApplication lhs rhs) =
  ExprApplication (populateVar varName varExpr lhs) (populateVar varName varExpr rhs)

run :: Expr -> Either String Expr
run expr@(ExprVariable name) = pure expr

run (ExprAbstraction name body) = do
  ranBody <- run body
  pure $ ExprAbstraction name ranBody

run (ExprApplication lhs@(ExprAbstraction lhsVar lhsBody) rhs) =
  run $ populateVar lhsVar rhs lhsBody

run (ExprApplication lhs rhs) = do
  ranLhs <- run lhs
  ranRhs <- run rhs

  if (ranLhs == lhs) && (ranRhs == rhs)
     then pure $ ExprApplication ranLhs ranRhs
     else run (ExprApplication ranLhs ranRhs)

eval :: String -> String
eval input =
  case result of
    Left(error)   -> error
    Right(output) -> output
  where
    result = do
      parsedResult <- generateAST input
      --execedResult <- run HM.empty parsedResult
      execedResult <- run parsedResult
      pure $ prettyPrint execedResult
