import AssemblyKeys._

name := "NEATER"

version := "0.1"

assemblySettings

jarName in assembly := "neater.jar"

scalaVersion := "2.10.3"

libraryDependencies += "org.scalaz" % "scalaz-core_2.10" % "7.0.5"

libraryDependencies += "edu.washington.cs.knowitall.common-scala" % "common-scala_2.10" % "1.1.2"

//unmanagedBase <<= baseDirectory { base => base / "lib" }
