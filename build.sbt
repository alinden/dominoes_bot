name := "alinden_dominoes_ai"
version := "0.01"
scalaVersion := "2.13.5"
description := "AI to play dominoes"
scalacOptions ++= Seq(
  "-Xlint:inaccessible",
  "-Xlint:adapted-args",
  "-Xlint:infer-any",
  "-Xlint:unused",
  "-deprecation",
  "-feature",
)
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.5.0",
  "org.scalatest" %% "scalatest" % "3.2.7" % "test"
)
