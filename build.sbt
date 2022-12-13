ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

val circeVersion = "0.14.3"

lazy val root = (project in file("."))
  .settings(
    name := "aoc2022",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.9.0"
    ) ++ Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % circeVersion)
  )
