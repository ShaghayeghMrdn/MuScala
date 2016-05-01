name := "ScalaMutation"

version := "1.0"

scalaVersion := "2.10.6"

libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "com.eed3si9n" %% "treehugger" % "0.4.1",
  "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.2"
    )