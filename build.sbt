name := "qf"

scalaVersion := "2.12.21"

organization := "org.stingray.contester"

maintainer := "i@stingr.net"

version := "2025.1"

scalacOptions ++= Seq(
  "-Xfatal-warnings",  // New lines for each options
  "-Xasync",
  "-deprecation",
  "-unchecked",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:existentials",
  "-language:postfixOps",
)

lazy val root = (project in file(".")).enablePlugins(PlayScala)

// Digest the assets on stage/dist (Assets.versioned then serves fingerprinted, cacheable URLs).
pipelineStages := Seq(digest)

resolvers ++= Seq(
  Resolver.mavenLocal,
  Resolver.jcenterRepo,
  "Atlassian Releases" at "https://maven.atlassian.com/public/"
) ++ Resolver.sonatypeOssRepos("snapshots")

updateOptions := updateOptions.value.withCachedResolution(true)

val spireVersion = "0.13.0"

val silhouetteVersion = "7.0.0"

val slickPG = "0.21.1"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.19" % Test,
  "com.typesafe.akka" %% "akka-testkit" % "2.6.21" % Test,
  "com.typesafe.play" %% "play-test" % play.core.PlayVersion.current % Test,
  "com.typesafe.play" %% "play-slick" % "5.1.0",
  "com.github.tototoshi" %% "slick-joda-mapper" % "2.4.2",
  "com.softwaremill.macwire" %% "macros" % "2.4.1" % "provided",
  "javax.xml.bind" % "jaxb-api" % "2.3.0",
  "com.adrianhurt" %% "play-bootstrap" % "1.6.1-P28-B3",
  cacheApi,
  ws,
  "org.webjars" %% "webjars-play" % "2.7.3",
  "org.webjars" % "bootstrap" % "3.4.1",
  "org.webjars" % "font-awesome" % "5.9.0",
  "org.webjars" % "bootstrap-datepicker" % "1.9.0",
  "org.webjars.npm" % "arrive" % "2.4.1",
  "org.webjars" % "momentjs" % "2.24.0",
  "org.webjars" % "bootstrap-material-design" % "0.5.10",
  "org.webjars" % "jquery-validation" % "1.19.0",
  "org.webjars.bower" % "roboto-fontface" % "0.7.0",
  "org.postgresql" % "postgresql" % "42.7.14",
  "commons-io" % "commons-io" % "2.22.0",
  "com.github.nscala-time" %% "nscala-time" % "2.24.0",
  "com.googlecode.htmlcompressor" % "htmlcompressor" % "1.5.2",
  "org.mozilla" % "rhino" % "1.7.15",
  "org.scala-lang.modules" %% "scala-async" % "1.0.1",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided,
  "org.typelevel" %% "cats-core" % "2.1.1",
  "com.github.fkoehler" %% "play-html-compressor" % "2.8.0",
  "com.mohiva" %% "play-silhouette" % silhouetteVersion,
  "com.mohiva" %% "play-silhouette-password-bcrypt" % silhouetteVersion,
  "com.mohiva" %% "play-silhouette-persistence" % silhouetteVersion,
  "com.mohiva" %% "play-silhouette-crypto-jca" % silhouetteVersion,
  "org.spire-math" %% "spire" % spireVersion,
  "org.spire-math" %% "spire-extras" % spireVersion,
  "info.faljse" % "SDNotify" % "1.3",
  "com.google.guava" % "guava" % "33.8.0-jre",
  "com.github.tminglei" %% "slick-pg" % slickPG,
  "com.github.tminglei" %% "slick-pg_joda-time" % slickPG,
  "com.github.tminglei" %% "slick-pg_play-json" % slickPG
).map(_.exclude("com.zaxxer", "HikariCP-java6"))

routesGenerator := InjectedRoutesGenerator

// Exclude commons-logging because it conflicts with the jcl-over-slf4j
libraryDependencies ~= { _ map {
  case m if m.organization == "commons-logging" =>
    m.exclude("commons-logging", "commons-logging")
  case m => m
}}

Compile / PB.targets := Seq(
  scalapb.gen() -> (Compile / sourceManaged).value
)

Compile / javacOptions ++= Seq("--release", "17")
// The DB specs share one database; run suites one at a time.
Test / parallelExecution := false
