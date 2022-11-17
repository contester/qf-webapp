name := "qf"

scalaVersion := "2.12.17"

organization := "org.stingray.contester"

maintainer := "i@stingr.net"

version := "2022.1"

scalacOptions ++= Seq(
  "-Xfatal-warnings",  // New lines for each options
  "-Xasync",
  "-deprecation",
  "-unchecked",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:existentials",
  "-language:postfixOps",
  "-opt:l:method",
  "-opt:l:inline",
  "-opt-inline-from:<sources>"
)

lazy val root = (project in file(".")).enablePlugins(PlayScala)

resolvers ++= Seq(
  Resolver.mavenLocal,
  Resolver.jcenterRepo,
  Resolver.sonatypeRepo("snapshots"),
  "Atlassian Releases" at "https://maven.atlassian.com/public/"
)

updateOptions := updateOptions.value.withCachedResolution(true)

val spireVersion = "0.13.0"

val silhouetteVersion = "7.0.0"

val slickPG = "0.19.2"

libraryDependencies ++= Seq(
  "org.stingray.contester" %% "contester-dbmodel" % "2022.0.1-SNAPSHOT",
  "com.typesafe.play" %% "play-slick" % "4.0.2",
  "com.github.tototoshi" %% "slick-joda-mapper" % "2.4.2",
  "com.softwaremill.macwire" %% "macros" % "2.4.1" % "provided",
  "javax.xml.bind" % "jaxb-api" % "2.3.0",
  "org.mariadb.jdbc" % "mariadb-java-client" % "3.1.0",
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
  "org.apache.httpcomponents" % "httpclient" % "4.5.12",
  "org.apache.httpcomponents" % "httpcore" % "4.4.13",
  "org.postgresql" % "postgresql" % "42.2.14",
  "commons-io" % "commons-io" % "2.11.0",
  "com.github.nscala-time" %% "nscala-time" % "2.24.0",
  "com.googlecode.htmlcompressor" % "htmlcompressor" % "1.5.2",
  "rhino" % "js" % "1.7R2",
  "org.scala-lang.modules" %% "scala-async" % "1.0.1",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided,
  "org.typelevel" %% "cats-core" % "2.1.1",
  "com.github.fkoehler" %% "play-html-compressor" % "2.8.0",
  "com.mohiva" %% "play-silhouette" % silhouetteVersion,
  "com.mohiva" %% "play-silhouette-password-bcrypt" % silhouetteVersion,
  "com.mohiva" %% "play-silhouette-persistence" % silhouetteVersion,
  "com.mohiva" %% "play-silhouette-crypto-jca" % silhouetteVersion,
  "com.github.seancfoley" % "ipaddress" % "5.3.1",
  "org.spire-math" %% "spire" % spireVersion,
  "org.spire-math" %% "spire-extras" % spireVersion,
  "info.faljse" % "SDNotify" % "1.3",
  "com.google.guava" % "guava" % "23.0",
  "com.github.tminglei" %% "slick-pg" % slickPG,
  "com.github.tminglei" %% "slick-pg_joda-time" % slickPG,
  "com.github.tminglei" %% "slick-pg_play-json" % slickPG
).map(_.exclude("com.zaxxer", "HikariCP-java6"))

val opRabbitVersion = "2.1.0"

libraryDependencies ++= Seq(
  "com.spingo" %% "op-rabbit-core"        % opRabbitVersion,
  "com.spingo" %% "op-rabbit-play-json"   % opRabbitVersion,
  "com.spingo" %% "op-rabbit-json4s"      % opRabbitVersion,
  "com.spingo" %% "op-rabbit-airbrake"    % opRabbitVersion,
  "com.spingo" %% "op-rabbit-akka-stream" % opRabbitVersion
)

routesGenerator := InjectedRoutesGenerator

// Exclude commons-logging because it conflicts with the jcl-over-slf4j
libraryDependencies ~= { _ map {
  case m if m.organization == "commons-logging" =>
    m.exclude("commons-logging", "commons-logging")
  case m => m
}}

PB.targets in Compile := Seq(
  scalapb.gen() -> (sourceManaged in Compile).value
)

javacOptions in Compile ++= Seq("-source", "1.8",  "-target", "1.8")