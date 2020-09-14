name := "WUST-DNA-Akkaless"

version := "0.1"
scalacOptions += "-deprecation"
scalaVersion := "2.13.3"

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"

resolvers += Resolver.bintrayRepo("cibotech", "public")
libraryDependencies += "com.cibo" %% "evilplot" % "0.8.0"

val circeVersion = "0.12.3"
libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)