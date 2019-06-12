scalaVersion := "2.12.8"

libraryDependencies += "io.monix" %% "minitest" % "2.4.0" % "test"
libraryDependencies += "io.monix" %% "minitest-laws" % "2.4.0" % "test"

testFrameworks += new TestFramework("minitest.runner.Framework")