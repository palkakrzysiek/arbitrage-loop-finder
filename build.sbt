name := "arbitrage"

version := "0.1"

scalaVersion := "2.13.8"

idePackagePrefix := Some("com.kpalka")

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.11"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.11" % "test"

libraryDependencies += "com.lihaoyi" %% "upickle" % "1.5.0"
libraryDependencies += "com.lihaoyi" %% "requests" % "0.7.0"
