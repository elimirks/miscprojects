val scala3Version = "3.1.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "MIT Press Introduction To Algorithms Solutions",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % Test
  )
