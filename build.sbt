
name := "beanbakery"

version := "0.1"

scalaVersion := "2.9.1"


resolvers += "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/"



libraryDependencies += "org.scalatest" %% "scalatest" % "1.7.1" % "test"


libraryDependencies += "org.slf4j" % "slf4j-api" % "1.6.4"

libraryDependencies += "org.slf4j" % "slf4j-log4j12" % "1.6.4"

libraryDependencies += "log4j" % "log4j" % "1.2.16"


libraryDependencies += "com.dyuproject.protostuff" % "protostuff-api" % "1.0.4"

libraryDependencies += "com.dyuproject.protostuff" % "protostuff-runtime" % "1.0.4"

libraryDependencies += "com.dyuproject.protostuff" % "protostuff-core" % "1.0.4"

libraryDependencies += "com.dyuproject.protostuff" % "protostuff-collectionschema" % "1.0.4"


libraryDependencies += "org.yaml" % "snakeyaml" % "1.11-SNAPSHOT"


libraryDependencies += "org.scalastuff" % "scalabeans" % "0.3"

// Parboiled for parser generation
libraryDependencies += "org.parboiled" % "parboiled-scala" % "1.0.2"
