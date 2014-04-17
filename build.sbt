name := "RSASim"

scalaVersion := "2.10.2"

scalacOptions in Test ++= Seq("-Yrangepos")

resolvers ++= Seq(
	  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
	  "releases"  at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
		    "org.specs2" %% "specs2" % "2.2" % "test"
                    , "com.typesafe" % "scalalogging-log4j_2.10" % "1.0.1"
)

fork in run := true

connectInput in run := true
