module Interpreter.Runner where

import Prelude

import Data.Either (Either(..))
import Data.HashSet as HS
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

run :: HS.HashSet String -> Expr -> Either String Expr
run scope expr@(ExprVariable name) =
  if HS.member name scope
    then pure expr
    else Left $ "Undefined variable: " <> name

run scope (ExprAbstraction var body) = do
  ranBody <- run (HS.insert var scope) body
  pure $ ExprAbstraction var ranBody

run scope (ExprApplication lhs@(ExprAbstraction lhsVar lhsBody) rhs) =
  run scope $ populateVar lhsVar rhs lhsBody

run scope (ExprApplication lhs rhs) = do
  ranLhs <- run scope lhs
  ranRhs <- run scope rhs

  if (ranLhs == lhs) && (ranRhs == rhs)
     then pure $ ExprApplication ranLhs ranRhs
     else run scope (ExprApplication ranLhs ranRhs)

eval :: String -> String
eval input =
  case result of
    Left(error)   -> error
    Right(output) -> output
  where
    result = do
      parsedResult <- generateAST input
      --execedResult <- run HM.empty parsedResult
      execedResult <- run HS.empty parsedResult
      pure $ prettyPrint execedResult
