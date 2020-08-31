module Test.Main where

import Prelude

import Effect (Effect)

import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

import Interpreter.Runner (eval)

main :: Effect Unit
main = do
  runTest do
    suite "Eval tests" do
      test "Shadowed identity" do
        Assert.equal "λx.x"
          $ eval "(λx.λx.x λy.y)"

      test "Succ 0" do
        Assert.equal "λf.λx.(f x)"
          $ eval """
                 0 := \f.\x.x
                 succ := \n.\f.\x.(f ((n f) x))
                 (succ 0)
                 """

      test "Succ Succ 0" do
        Assert.equal "λf.λx.(f (f x))"
          $ eval """
                 succ := \n.\f.\x.(f ((n f) x))
                 (succ λf.λx.(f x))
                 """

      {-
      test "Add 1 + 2" do
        Assert.equal "λf.λx.(f (f x))"
          $ eval """
                 0 := \f.\x.x
                 1 := \f.\x.(f x)
                 2 := λf.λx.(f (f x))
                 plus := λm.λn.λf.λx.(m f (n f x))

                 (plus 1 2)
                 """
      -}
