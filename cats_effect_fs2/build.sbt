val scala3Version = "3.0.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala3-simple",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.6.1",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.1.1",
    libraryDependencies += "co.fs2" %% "fs2-core" % "3.0.5",
    libraryDependencies += "co.fs2" %% "fs2-io" % "3.0.5",
  )
