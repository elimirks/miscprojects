module Test.Main where

import Prelude

import Effect (Effect)
import Interpreter.Parser (Expr(..), generateAST)
import Interpreter.Runner (eval)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = do
  runTest do
    suite "Parse tests" do
      test "Variable equality" do
        Assert.equal true
          $ (ExprVariable "24") == (ExprVariable "24")

        Assert.equal false
          $ (ExprVariable "24") == (ExprVariable "42")

      test "Abstraction equality" do
        Assert.equal true
          $ (ExprAbstraction "foo" (ExprVariable "bar")) ==
            (ExprAbstraction "foo" (ExprVariable "bar"))

        Assert.equal false
          $ (ExprAbstraction "foo" (ExprVariable "bar")) ==
            (ExprAbstraction "oof" (ExprVariable "bar"))

        Assert.equal false
          $ (ExprAbstraction "foo" (ExprVariable "bar")) ==
            (ExprAbstraction "foo" (ExprVariable "rab"))

      test "Abstraction equality" do
        Assert.equal true
          $ (ExprApplication (ExprVariable "foo") (ExprVariable "bar")) ==
            (ExprApplication (ExprVariable "foo") (ExprVariable "bar"))

        Assert.equal false
          $ (ExprApplication (ExprVariable "foo") (ExprVariable "bar")) ==
            (ExprApplication (ExprVariable "oof") (ExprVariable "bar"))

        Assert.equal false
          $ (ExprApplication (ExprVariable "foo") (ExprVariable "bar")) ==
            (ExprApplication (ExprVariable "foo") (ExprVariable "rab"))

      test "Parsed equality" do
        Assert.equal true
          $ generateAST "((n f) ((n f) x))" == generateAST "((n f) ((n f) x))"

    suite "Eval tests" do
      test "Shadowed identity" do
        Assert.equal "λx.x"
          $ eval "(λx.λx.x λy.y)"

      test "Succ 0" do
        Assert.equal "λf.λx.(f x)"
          $ eval """
                 0 := \f.\x.x
                 succ := \n.\f.\x.(f (n f x))
                 (succ 0)
                 """

      test "Rebinding in scope exprs" do
        Assert.equal "λf.λx.(f x)"
          $ eval """
                 0 := \f.\x.x
                 succ := \n.\f.\x.(f (n f x))
                 bazinga := \q.(succ 0)
                 0 := \f.\x.(f x)

                 (bazinga \i.i)
                 """

      test "Succ Succ 0" do
        Assert.equal "λf.λx.(f (f x))"
          $ eval """
                 succ := \n.\f.\x.(f (n f x))
                 (succ λf.λx.(f x))
                 """

      test "Add 1 + 0" do
        Assert.equal "λf.λx.(f x)"
          $ eval """
                 0 := \f.\x.x
                 1 := \f.\y.(f y)
                 plus := λm.λn.λf.λx.(m f (n f x))

                 (plus 1 0)
                 """

      test "Add 1 + 0 shadow jago edition" do
        Assert.equal "λf.λx.(f x)"
          $ eval """
                 0 := \f.\x.x
                 1 := \f.\x.(f x)
                 plus := λm.λn.λf.λx.(m f (n f x))

                 (plus 1 0)
                 """

      test "Not allow undefined variables" do
        Assert.equal "Undefined variable: x"
          $ eval "(x y)"

      test "Allow constant to use previous constants" do
        Assert.equal "λx.x"
          $ eval """
                 id1 := \y.y
                 id2 := id1

                 (id2 \x.x)
                 """

      test "Multiply 2 * 3" do
        Assert.equal "λf.λx.(f (f (f (f (f (f x))))))"
          $ eval """
                 2 := λf.λx.(f (f x))
                 3 := λf.λx.(f (f (f x)))
                 mult := \m.\n.\f.(m (n f))

                 (mult 2 3)
                 """

      test "Multiply 2 * 3" do
        Assert.equal "λf.λx.(f (f (f (f (f (f x))))))"
          $ eval """
                 2 := λf.λx.(f (f x))
                 3 := λf.λx.(f (f (f x)))
                 mult := \m.\n.\f.(m (n f))

                 (mult 2 3)
                 """

      test "Pred 3" do
        Assert.equal "λf.λx.(f (f x))"
          $ eval """
                 2 := λf.λx.(f (f x))
                 3 := λf.λx.(f (f (f x)))
                 pred := λn.λf.λx.(n (λg.λh.(h (g f))) λu.x λu.u)

                 (pred 3)
                 """

      test "Pred succ succ pred 2" do
        Assert.equal "λf.λx.(f (f x))"
          $ eval """
                 2 := λf.λx.(f (f x))
                 succ := \n.\f.\x.(f (n f x))
                 pred := λn.λf.λx.(n (λg.λh.(h (g f))) λu.x λu.u)

                 (pred (succ (succ (pred 2))))
                 """
