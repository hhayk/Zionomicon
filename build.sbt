ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

// https://mvnrepository.com/artifact/dev.zio/zio
val zioVersion = "2.0.19"

libraryDependencies += "dev.zio" %% "zio" % zioVersion
libraryDependencies += "dev.zio" %% "zio-test" % zioVersion
libraryDependencies += "dev.zio" %% "zio-test-sbt" % zioVersion

lazy val root = (project in file("."))
  .settings(
    name := "Zionomicon"
  )
