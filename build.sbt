name := "milenio"

version := "1.0"

lazy val `milenio` = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.7"

libraryDependencies ++= Seq( jdbc , anorm , cache , ws )

libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "2.6.0"

libraryDependencies += "com.typesafe.play" %% "play-mailer" % "2.4.1"

unmanagedResourceDirectories in Test <+=  baseDirectory ( _ /"target/web/public/test" )


fork in run := true