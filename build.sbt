name := "sample-gamer"

scalaVersion := "2.10.4"

javacOptions ++= Seq("-source", "1.7", "-target", "1.7")

resolvers += "TPS Repository" at "http://w3.maven.monsanto.com/nexus/content/groups/tps"

libraryDependencies ++= Seq(
//  "com.monsanto.tps.microbials" % "play-security" % "1.0.3",
  "org.mockito" % "mockito-all" % "1.8.4",
//  "com.google.guava" % "guava-parent" % "17.0",
  "com.monsanto.tps.scala" %% "scala-test-support" % "1.2.1" % "test",
  "org.scalatestplus" %% "play" % "1.2.0" % "test"
)
