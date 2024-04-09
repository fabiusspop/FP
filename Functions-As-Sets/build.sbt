ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

libraryDependencies += "org.scalameta" %% "munit" % "0.7.26" % Test

lazy val root = (project in file("."))
  .settings(
    name := "Homework-1-Functions-as-sets"
  )
