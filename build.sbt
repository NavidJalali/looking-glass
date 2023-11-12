ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / organization := "io.navidjalali"
ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "looking-glass",
    libraryDependencies ++= Seq(
      "io.swagger.parser.v3" % "swagger-parser" % "2.1.18",
      "org.scalameta"       %% "munit"          % "0.7.29" % Test
    ),
    scalacOptions ++= Seq(
      "-Xfatal-warnings",
      "-unchecked",
      "-Wunused:implicits",
      "-Wunused:imports",
      "-Wunused:locals",
      "-Wunused:params",
      "-Wunused:privates",
      "-Wvalue-discard",
      "-deprecation"
    )
  )
