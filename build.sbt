initialCommands in console := "import opt._"

scalaVersion := "2.11.0-M6"

name := "opt"

scalacOptions ++= Seq("-optimize", "-deprecation", "-unchecked", "-Xlint")
