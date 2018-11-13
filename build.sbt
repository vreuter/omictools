name := "omictools"
version := "0.1.0-SNAPSHOT"
organization := "vreuter"
scalaVersion := "2.12.7"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.4.0"
libraryDependencies += "org.typelevel" % "mouse_2.12" % "0.19"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0"
//libraryDependencies += "org.typelevel" %% "spire" % "0.14.1"

publishTo := Some(
  Resolver.file("omictools", new File(Path.userHome.absolutePath + "/.m2/repository")))

libraryDependencies += "eu.timepit" %% "refined" % "0.9.2"                  // Refinement types

// Generic numeric types
//libraryDependencies += "org.typelevel" %% "spire" % "0.16.0"

/* Locals */
resolvers += Resolver.mavenLocal
libraryDependencies += "vreuter" %% "experic" % "0.0.2-SNAPSHOT"

/* Testing */
resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

excludeFilter in unmanagedSources := HiddenFileFilter || "tmp*.scala"

