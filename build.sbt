name := """freet-presentation"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.8"

resolvers in ThisBuild ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

addCompilerPlugin("org.spire-math" %% "kind-projector"  % "0.7.1")
addCompilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full)

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
  "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.1" % Test,
  "org.typelevel" %% "cats" % "0.7.2"
)

