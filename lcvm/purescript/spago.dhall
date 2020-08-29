{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "lcvm"
, dependencies = [ "console", "effect", "psci-support", "maybe", "tuples", "lists", "arrays", "strings", "unicode", "debug" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
