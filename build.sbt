name := "qf"

scalaVersion  := "2.11.7"

organization := "org.stingray.contester"

version := "0.0.1-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

mainClass in assembly := Some("play.core.server.NettyServer")

fullClasspath in assembly += Attributed.blank(PlayKeys.playPackageAssets.value)

// Take the first ServerWithStop because it's packaged into two jars
assemblyMergeStrategy in assembly := {
  case PathList("play", "core", "server", "ServerWithStop.class") => MergeStrategy.first
  case other => (assemblyMergeStrategy in assembly).value(other)
}

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

libraryDependencies ++= Seq(
  cache,
  "com.adrianhurt" %% "play-bootstrap3" % "0.4.4-P24",
  "org.webjars" % "font-awesome" % "4.3.0-1",
  "org.webjars" % "bootstrap-datepicker" % "1.3.1",
  "jp.t2v" %% "play2-auth"      % "0.14.0",
  "jp.t2v" %% "play2-auth-test" % "0.14.0" % "test",
  "com.typesafe.play" %% "play-slick" % "1.0.0",
  "mysql" % "mysql-connector-java" % "5.1.36",
  "commons-io" % "commons-io" % "2.4",
  "com.github.nscala-time" %% "nscala-time" % "2.0.0",
  "org.scala-lang.modules" %% "scala-async" % "0.9.4",
  "org.spire-math" %% "spire" % "0.10.1"
)

resolvers ++= Seq(
  "SpinGo OSS" at "http://spingo-oss.s3.amazonaws.com/repositories/releases"
)

val opRabbitVersion = "1.0.0-M11"

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

