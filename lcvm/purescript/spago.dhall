{ name = "halogen-project"
, dependencies = [ "console", "effect", "halogen", "psci-support", "maybe", "tuples", "lists", "arrays", "strings", "unicode", "debug", "test-unit", "unordered-collections"  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
