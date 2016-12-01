name := "CellularAutomaton"

version := "1.0"

scalaVersion := "2.12.0"

val scalazVersion = "7.2.7"

libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % scalazVersion,
    "org.scalaz" %% "scalaz-effect" % scalazVersion
)
libraryDependencies += "org.scala-lang.modules" % "scala-swing_2.11" % "1.0.1"
