name := "qf"

scalaVersion  := "2.11.8"

organization := "org.stingray.contester"

version := "0.2"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-optimise", "-explaintypes", "-Xcheckinit",
  "-Xlint", "-feature")

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

updateOptions := updateOptions.value.withCachedResolution(true)

val spireVersion = "0.12.0"

libraryDependencies ++= Seq(
  cache,
  ws,
  "com.adrianhurt" %% "play-bootstrap" % "1.0-P25-B3",
  "org.webjars" % "font-awesome" % "4.6.3",
  "org.webjars" % "bootstrap-datepicker" % "1.6.1",
  "org.apache.httpcomponents" % "httpclient" % "4.5.2",
  "org.apache.httpcomponents" % "httpcore" % "4.4.5",
  "jp.t2v" %% "play2-auth"      % "0.14.2",
  "jp.t2v" %% "play2-auth-test" % "0.14.2" % "test",
  "com.typesafe.play" %% "play-slick" % "2.0.2",
  "com.typesafe.play" %% "play-slick-evolutions" % "2.0.2",
  "org.mariadb.jdbc" % "mariadb-java-client" % "1.4.6",
  "commons-io" % "commons-io" % "2.5",
  "com.github.nscala-time" %% "nscala-time" % "2.12.0",
  "com.googlecode.htmlcompressor" % "htmlcompressor" % "1.5.2",
  "rhino" % "js" % "1.7R2",
  "org.spire-math" %% "spire" % spireVersion,
  "org.spire-math" %% "spire-extras" % spireVersion,
  "com.google.guava" % "guava" % "19.0"
)

resolvers ++= Seq(
  "SpinGo OSS" at "http://spingo-oss.s3.amazonaws.com/repositories/releases"
)

val opRabbitVersion = "1.3.0"

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

