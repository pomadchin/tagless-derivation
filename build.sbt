name         := "tagless-derivation"
version      := "0.1.0-SNAPSHOT"
scalaVersion := "3.2.1"
organization := "com.pomadchin"
scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-language:implicitConversions",
  "-language:reflectiveCalls",
  "-language:higherKinds",
  "-language:postfixOps",
  "-language:existentials",
  "-feature",
  "-source:future",
  "-explain"
)

scalacOptions ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((3, _))       => Seq("-Ykind-projector:underscores")
    case Some((2, 12 | 13)) => Seq("-Xsource:3", "-P:kind-projector:underscore-placeholders")
    case _                  => Nil
  }
}

resolvers ++= Resolver.sonatypeOssRepos("releases") ++ Resolver.sonatypeOssRepos("snapshots")

fork := true

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.8.0",
  "org.scalatest" %% "scalatest" % "3.2.13" % Test
)
