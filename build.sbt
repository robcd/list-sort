scalaVersion := "2.10.0-RC2"

scalacOptions += "-feature"

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "org.scalatest" % "scalatest_2.10.0-RC2" % "1.8-B2"

libraryDependencies += "com.github.axel22" %% "scalameter" % "0.2"

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

logBuffered := false
