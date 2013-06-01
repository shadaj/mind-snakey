organization := "me.shadaj.neuro"

name := "snakey"

version := "0.1"

scalaVersion := "2.10.0"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "me.shadaj.neuro" %% "thinkgear" % "0.1"

libraryDependencies += "me.shadaj.neuro" %% "io" % "0.1"
 
libraryDependencies +=
  "com.typesafe.akka" %% "akka-actor" % "2.1.4"