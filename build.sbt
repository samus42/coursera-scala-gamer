lazy val root = (project in file(".")).enablePlugins(JavaAppPackaging)

name := "scott-coursera-gamer"

scalaVersion := "2.11.4"

javacOptions ++= Seq("-source", "1.7", "-target", "1.7")

//libraryDependencies ++= Seq(
//  "com.monsanto.tps.microbials" % "play-security" % "1.0.3",
//  "org.mockito" % "mockito-all" % "1.8.4",
//  "org.scalaz" %% "scalaz-core" % "7.1.1"
//  "com.google.guava" % "guava-parent" % "17.0",
//  "com.monsanto.tps.scala" %% "scala-test-support" % "1.2.1" % "test",
//  "org.scalatestplus" %% "play" % "1.2.0" % "test"
//)
