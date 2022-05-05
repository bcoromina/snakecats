ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "snakecats"
  )

scalaVersion := "2.13.6"
libraryDependencies += "org.typelevel" %% "cats-effect" % "3.3.5"
//libraryDependencies += "io.monix" %% "monix" % "3.4.0"