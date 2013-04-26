name := "tshrdlu"

version := "0.1.5-SNAPSHOT"

organization := "edu.utexas"

scalaVersion := "2.10.1"

crossPaths := false

retrieveManaged := true

resolvers ++= Seq(
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  "sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "sonatype releases" at "https://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "org.scalanlp" % "nak" % "1.1.2",
  "org.scalanlp" % "chalk" % "1.1.3-SNAPSHOT",
  "org.twitter4j" % "twitter4j-core" % "3.0.3",
  "org.twitter4j" % "twitter4j-stream" % "3.0.3",
  "com.typesafe.akka" %% "akka-actor" % "2.1.2",
  "commons-codec" % "commons-codec" % "1.7",
  "org.apache.lucene" % "lucene-core" % "4.2.0",
  "org.apache.lucene" % "lucene-analyzers-common" % "4.2.0",
  "org.apache.lucene" % "lucene-queryparser" % "4.2.0",
  "org.apache.httpcomponents" % "httpclient" % "4.1.2",
  "com.google.code.gson" % "gson" % "1.7.1"
)
