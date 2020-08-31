module Main where

import Prelude

import Data.Either (Either(..))
import Data.HashMap as HM
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Debug.Trace (trace)
import Effect (Effect)
import Effect.Console (log)
import Parser (Expr(..), Variable(..), generateAST)

prettyPrint :: Expr -> String
prettyPrint (ExprVariable (Variable name)) = name
prettyPrint (ExprApplication target param) =
  "(" <> prettyPrint target <> " " <> prettyPrint param <> ")"
prettyPrint (ExprAbstraction (Variable arg) body) =
  "λ" <> arg <> "." <> prettyPrint body


data Definition = Unbound | Bound Expr

type Scope = HM.HashMap String Definition

run :: (Tuple Scope Expr) -> Either String Expr
run (Tuple scope exprVar@(ExprVariable (Variable name))) =
  case (HM.lookup name scope) of
    Nothing            -> Left $ "Unbound variable: " <> name
    Just (Unbound)     -> Right exprVar
    Just (Bound value) -> Right value

run (Tuple scope exprAbs@(ExprAbstraction (Variable varName) body)) =
  case (HM.lookup varName scope) of
    Nothing -> do
      ranBody <- run $ Tuple (HM.insert varName Unbound scope) body
      pure $ ExprAbstraction (Variable varName) ranBody
    Just (Unbound) -> Left $ "Shadowed variable: " <> varName
    Just (Bound value) -> run (Tuple scope body)

run (Tuple scope app@(ExprApplication target param)) =
  do
    ranTarget <- run (Tuple scope target)
    ranParam  <- run (Tuple scope param)
    
    case (Tuple ranTarget ranParam) of
      (Tuple (ExprAbstraction (Variable v) body) (ExprAbstraction _ _)) ->
        if (HM.member v scope)
           then Left $ "Variable " <> v <> " is already in scope"
           else run (Tuple (HM.insert v (Bound ranParam) scope) body)
      _ -> Left $ "Expected abstraction at " <> prettyPrint app

main :: Effect Unit
main = do
  log $ show do
    parsedResult <- generateAST "#bar \n id := (\\x.x)\n id\n  #foo"
    execedResult <- run (Tuple HM.empty parsedResult)
    pure $ prettyPrint execedResult

  log $ show do
    parsedResult <- generateAST "\\z.(\\x.x \\y.y)"
    execedResult <- run (Tuple HM.empty parsedResult)
    pure $ prettyPrint execedResult
