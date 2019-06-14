name := "qf"

scalaVersion := "2.12.8"

organization := "org.stingray.contester"

version := "2018.6"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

resolvers ++= Seq(
  Resolver.jcenterRepo,
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Atlassian Releases" at "https://maven.atlassian.com/public/",
  "SpinGo OSS" at "http://spingo-oss.s3.amazonaws.com/repositories/releases"
)

updateOptions := updateOptions.value.withCachedResolution(true)

val spireVersion = "0.13.0"

val silhouetteVersion = "5.0.7"

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-slick" % "3.0.3",
  "com.softwaremill.macwire" %% "macros" % "2.3.1" % "provided",
  "javax.xml.bind" % "jaxb-api" % "2.3.0",
  "com.adrianhurt" %% "play-bootstrap" % "1.4-P26-B3-SNAPSHOT",
  cacheApi,
  ws,
  "org.webjars" %% "webjars-play" % "2.6.3",
  "org.webjars" % "bootstrap" % "3.4.1-1",
  "org.webjars" % "font-awesome" % "5.1.0",
  "org.webjars" % "bootstrap-datepicker" % "1.6.4",
  "org.webjars.npm" % "arrive" % "2.4.1",
  "org.webjars" % "momentjs" % "2.24.0",
  "org.webjars" % "bootstrap-material-design" % "0.5.9",
  "org.webjars" % "jquery-validation" % "1.16.0",
  "org.webjars.bower" % "roboto-fontface" % "0.7.0",
  "org.apache.httpcomponents" % "httpclient" % "4.5.6",
  "org.apache.httpcomponents" % "httpcore" % "4.4.10",
  "org.mariadb.jdbc" % "mariadb-java-client" % "2.2.6",
  "commons-io" % "commons-io" % "2.6",
  "com.github.nscala-time" %% "nscala-time" % "2.22.0",
  "com.googlecode.htmlcompressor" % "htmlcompressor" % "1.5.2",
  "rhino" % "js" % "1.7R2",
  "org.scala-lang.modules" %% "scala-async" % "0.10.0",
  "com.typesafe.slick" %% "slick" % "3.2.3",
  "com.typesafe.slick" %% "slick-hikaricp" % "3.2.3",
  "org.typelevel" %% "cats" % "0.9.0",
  "com.mohiva" %% "play-silhouette" % silhouetteVersion,
  "com.mohiva" %% "play-silhouette-password-bcrypt" % silhouetteVersion,
  "com.mohiva" %% "play-silhouette-persistence" % silhouetteVersion,
  "com.mohiva" %% "play-silhouette-crypto-jca" % silhouetteVersion,
  "org.spire-math" %% "spire" % spireVersion,
  "org.spire-math" %% "spire-extras" % spireVersion,
  "info.faljse" % "SDNotify" % "1.1",
  "com.google.guava" % "guava" % "23.0"
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

