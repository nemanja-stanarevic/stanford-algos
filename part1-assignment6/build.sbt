name := """part1-assignment6"""

version := "1.0"

scalaVersion := "2.11.1"

Keys.fork in run := true

//javaOptions in run ++= Seq(
//    "-Xms1g",
//    "-Xmx2g"
//)

Keys.fork in Test := true

//javaOptions in Test ++= Seq(
//    "-Xms1g",
//    "-Xmx2g"
//)

// Change this to another test framework if you prefer
libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.6" % "test"

// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.3.3"

