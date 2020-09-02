module Interpreter.Runner where

import Prelude

import Control.Alternative (empty)
import Control.Monad.State (State, evalState, gets, modify)
import Data.Either (Either(..), isLeft)
import Data.Foldable (foldl, maximum)
import Data.HashMap as HM
import Data.Hashable (class Hashable, hash)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Interpreter.Parser (Expr(..), ExprS, exprVariableP, generateAST)

type Id = Int
data Variable = Variable String Id

instance showVariable :: Show Variable where
  show (Variable name _) = name
  --show (Variable name id) = "(" <> name <> "," <> show id <> ")"

instance eqVariable :: Eq Variable where
  eq (Variable a1 a2) (Variable b1 b2) = a1 == b1 && a2 == b2

instance hashableVariable :: Hashable Variable where
  hash :: Variable -> Int
  hash (Variable name id) = hash $ name <> "," <> show id

type ExprR = Expr Variable

prettyPrint :: forall a. Show a => (Expr a) -> String
prettyPrint (ExprApplication target param) =
  "(" <> prettyPrint target <> " " <> prettyPrint param <> ")"
prettyPrint (ExprAbstraction arg body) =
  "λ" <> show arg <> "." <> prettyPrint body
prettyPrint other = show other

data Definition = Unbound | Bound ExprR

instance eqDefinition :: Eq Definition where
  eq (Bound e1) (Bound e2) = e1 == e2
  eq Unbound Unbound = true
  eq _ _ = false

instance showDefinition :: Show Definition where
  show Unbound   = "unbound"
  show (Bound e) = show e

type Scope = HM.HashMap Variable Definition

showScope :: Scope -> String
showScope scope = foldl join "" $ prettify <$> HM.toArrayBy Tuple scope
  where
    join acc it = it <> ", " <> acc
    prettify :: Tuple Variable Definition -> String
    prettify (Tuple name def) = show name <> ": " <> show def

run :: Scope -> ExprR -> Either String ExprR
run scope expr@(ExprVariable var) =
  case HM.lookup var scope of
    Nothing            -> Left $ "Undefined variable: " <> show var
    Just (Bound value) -> Right value
    Just Unbound       -> Right expr

run scope expr@(ExprAbstraction var body) =
  do
    ranBody <- run (HM.insert var Unbound scope) body
    pure $ ExprAbstraction var ranBody

run scope expr@(ExprApplication (ExprAbstraction lhsVar lhsBody) rhs) =
  run (HM.insert lhsVar (Bound rhs) scope) lhsBody
  
run scope expr@(ExprApplication lhs rhs) =
  do
    ranLhs <- run scope lhs
    ranRhs <- run scope rhs

    if (ranLhs == lhs) && (ranRhs == rhs)
       then pure expr
       else run scope $ ExprApplication ranLhs ranRhs

type ShadowState = State (HM.HashMap String Int)

shadowStateLookup :: String -> ShadowState Int
shadowStateLookup name = gets \table ->
  case HM.lookup name table of
    Nothing  -> -1
    Just val -> val

shadowStateIncrement :: String -> ShadowState Int
shadowStateIncrement name = do
  currentVal <- shadowStateLookup name
  _ <- modify (\table -> HM.insert name (currentVal + 1) table)
  pure $ currentVal + 1

convert :: ExprS -> ShadowState ExprR
convert (ExprVariable name) = do
  id <- shadowStateLookup name
  pure $ ExprVariable (Variable name id)

convert (ExprAbstraction varName body) = do
  id       <- shadowStateIncrement varName
  evalBody <- convert body
  pure $ ExprAbstraction (Variable varName id) evalBody

convert (ExprApplication lhs rhs) = do
  evalLhs <- convert lhs
  evalRhs <- convert rhs
  pure $ ExprApplication evalLhs evalRhs

runConvert :: ExprS -> ExprR
runConvert exprs = evalState (convert exprs) HM.empty

eval :: String -> String
eval input =
  case result of
    Left(error)   -> error
    Right(output) -> output
  where
    result = do
      parsedResult <- generateAST input
      execedResult <- run HM.empty $ runConvert parsedResult
      pure $ prettyPrint execedResult
