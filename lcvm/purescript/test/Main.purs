module Test.Main where

import Main
import Parser
import Prelude

import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = do
  log $ "whoopsie"
  {-
  runTest do
    suite "charP" do
      test "Should match a character" do
        Assert.equal (Just (Tuple (stringToList "ello") 'h'))
          $ runParser (charP 'h') (stringToList "hello")
      test "Should mismatch a character" do
        Assert.equal Nothing
          $ runParser (charP 'h') (stringToList "goodbye")
      test "Should mismatch absent input" do
        Assert.equal Nothing
          $ runParser (charP 'h') Nil

    suite "ws" do
      test "Should match multiple whitespaces" do
        Assert.equal (Just (Tuple Nil (stringToList " \t\n ")))
          $ runParser ws (stringToList " \t\n ")

      test "Should match no whitespace" do
        Assert.equal (Just (Tuple (stringToList "abc") Nil))
          $ runParser ws (stringToList "abc")

    suite "notNull" do
      test "Ban null results in other parsers" do
        Assert.equal Nothing
          $ runParser (notNull ws) (stringToList "")

    suite "exprP" do
      test "Should parse a variable" do
        Assert.equal (Just (Tuple Nil (ExprVariable $ Variable "abc")))
          $ runParser exprP (stringToList "abc")
-}
