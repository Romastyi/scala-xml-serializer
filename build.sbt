name := "scala-xml-serializer"

organization := "play.tools.xml"

version := "0.2"

scalaVersion := "2.11.1"

resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.specs2" %% "specs2" % "2.3.11" % "test",
  "junit" % "junit" % "4.8" % "test",
  "com.typesafe.play" %% "play-functional" % "2.3.4"
)

