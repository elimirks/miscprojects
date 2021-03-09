lazy val root = (project in file(".")).
  settings(
    name := "scalambda",
    version := "1.0",
    scalaVersion := "2.12.10",
    mainClass in assembly := Some("example.Main")
  )
