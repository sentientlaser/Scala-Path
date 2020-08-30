name := "TootsiFrootsiIceCream"

version := "2.0.0"

scalaVersion := "2.13.0"

coverageEnabled := true
//
// scalastyleConfig := baseDirectory.value / "project/scalastyle-config.xml"
//
// (scalastyleConfig in Test) := baseDirectory.value / "project/scalastyle-test-config.xml"

libraryDependencies += "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.11.2"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test"

mainClass in (Compile, run) := Some("org.shl.rest.Test")
