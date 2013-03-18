import AssemblyKeys._

assemblySettings

jarName in assembly := "andysum.jar"

scalaVersion := "2.10.0"

resolvers += "codelds" at "https://code.lds.org/nexus/content/groups/main-repo"

unmanagedBase <<= baseDirectory { base => base / "lib" }

libraryDependencies += "org.scalaz" %% "scalaz-core" % "6.0.4"