// The Typesafe repository
// resolvers += "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/"

ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always

// Use the Play sbt plugin for Play projects
addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.8.20")

// addSbtPlugin("com.typesafe.sbt" % "sbt-jshint" % "1.0.4")

// addSbtPlugin("com.typesafe.sbt" % "sbt-rjs" % "1.0.9")

// addSbtPlugin("com.typesafe.sbt" % "sbt-coffeescript" % "1.0.0")