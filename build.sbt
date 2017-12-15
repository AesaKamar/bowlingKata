import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "aesakamar",
      scalaVersion := "2.12.4",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "bowlingScores",
    libraryDependencies ++= Seq(
      scalaTest % Test,
      "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
      "org.typelevel" %% "cats-core" % "1.0.0-RC1",
      "com.lihaoyi" %% "pprint" % "0.5.3"
    )
  )

scalacOptions += "-Ypartial-unification"
