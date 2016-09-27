organization := "com.example"

version := "0.1"

scalaVersion := "2.11.7"

libraryDependencies ++= {
  val akkaV = "2.4.1"
  val sprayV = "1.3.3"
  Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaV,
    "io.spray" %% "spray-io" % sprayV,
    "io.spray" %% "spray-can" % sprayV,
    "io.spray" %% "spray-http" % sprayV,
    "io.spray" %% "spray-util" % sprayV,
    "io.spray" %% "spray-client" % sprayV,
    "io.spray" %% "spray-routing" % sprayV,
    "io.spray" %% "spray-testkit" % sprayV % "test",
    "org.specs2" %% "specs2-core" % "2.3.11" % "test",
    "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
  )
}
