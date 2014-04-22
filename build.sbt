name := "RSASim"

scalaVersion := "2.10.2"

scalacOptions in Test ++= Seq("-Yrangepos")

resolvers ++= Seq(
	  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
	  "releases"  at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
		    "org.specs2" %% "specs2" % "2.2" % "test"
                    , "ch.qos.logback" % "logback-classic" % "1.0.1"
)

