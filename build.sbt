initialCommands in console := "import opt._"

scalaVersion := "2.11.0"

licenses += ("MIT License" -> url("http://www.opensource.org/licenses/mit-license.php"))

name := "opt"

scalacOptions ++= Seq("-optimize", "-deprecation", "-unchecked", "-Xlint")
