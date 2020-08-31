module Interpreter.Runner where

import Prelude

import Data.Either (Either(..))
import Data.HashMap as HM
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Debug.Trace (trace)
import Interpreter.Parser (Expr(..), Variable(..), exprVariableP, generateAST)

prettyPrint :: Expr -> String
prettyPrint (ExprVariable (Variable name)) = name
prettyPrint (ExprApplication target param) =
  "(" <> prettyPrint target <> " " <> prettyPrint param <> ")"
prettyPrint (ExprAbstraction (Variable arg) body) =
  "λ" <> arg <> "." <> prettyPrint body

data Definition = Unbound | Bound Expr

type Scope = HM.HashMap String Definition

run :: Scope -> Expr -> Either String Expr
run scope expr@(ExprVariable (Variable name)) =
  trace (prettyPrint expr) \_ ->
  case HM.lookup name scope of
    Nothing            -> Left $ "Undefined variable: " <> name
    Just (Bound value) -> Right value
    _                  -> Right expr

run scope expr@(ExprAbstraction var@(Variable varName) body) =
  trace (prettyPrint expr) \_ ->
  do
    ranBody <- run (HM.insert varName Unbound scope) body
    pure $ ExprAbstraction var ranBody

run scope expr@(ExprApplication (ExprAbstraction (Variable lhsVar) lhsBody) rhs) =
  trace (prettyPrint expr) \_ ->
  do
    ranRhs <- run scope rhs
    run (HM.insert lhsVar (Bound ranRhs) scope) lhsBody

run scope expr@(ExprApplication lhs rhs) =
  trace (prettyPrint expr) \_ ->
  do
    ranLhs <- run scope lhs
    ranRhs <- run scope rhs

    if (ranLhs == lhs) && (ranRhs == rhs)
       then pure expr
       else run scope $ ExprApplication ranLhs ranRhs

eval :: String -> String
eval input =
  case result of
    Left(error)   -> error
    Right(output) -> output
  where
    result = do
      parsedResult <- generateAST input
      execedResult <- run HM.empty parsedResult
      pure $ prettyPrint execedResult
