addDependencyTreePlugin
addSbtPlugin("com.timushev.sbt" % "sbt-updates"  % "0.6.3")
addSbtPlugin("org.scalameta"    % "sbt-scalafmt" % "2.4.6")

libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
