name := "algorithms"
version := "0.0.1-SNAPSHOT"

scalaVersion := "2.13.1"

scalacOptions ++= Seq(
  "-encoding", "UTF-8",   // source files are in UTF-8
  "-deprecation",         // warn about use of deprecated APIs
  "-unchecked",           // warn about unchecked type parameters
  "-feature",             // warn about misused language features
  "-language:higherKinds",// allow higher kinded types without `import scala.language.higherKinds`
  "-Xlint"               // enable handy linter warnings
  //"-Yopt-inline-heuristics" dont example during development
    //"-Xfatal-warnings",     // turn compiler warnings into errors
  //"-Ypartial-unification" //by default in scala 2.13
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.1.1",
  "org.scalatest" %% "scalatest-wordspec" % "3.2.0" % Test
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)
