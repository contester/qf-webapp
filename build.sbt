name := "qf"

scalaVersion  := "2.11.11"

organization := "org.stingray.contester"

version := "0.2"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-optimise", "-explaintypes", "-Xcheckinit",
  "-Xlint", "-feature")

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

updateOptions := updateOptions.value.withCachedResolution(true)

scalaJSProjects := Seq(client)

pipelineStages in Assets := Seq(scalaJSPipeline)

lazy val client = (project in file("client")).settings(
  scalaVersion := "2.11.11",
  persistLauncher := true,
  persistLauncher in Test := false,
  libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "0.9.1",
    "org.querki" %%% "jquery-facade" % "1.0-RC6"
  )
).enablePlugins(ScalaJSPlugin, ScalaJSWeb)

val spireVersion = "0.13.0"

// pipelineStages := Seq(rjs)

libraryDependencies ++= Seq(
  cache,
  ws,
  "com.adrianhurt" %% "play-bootstrap" % "1.1.1-P25-B3",
  "org.webjars" %% "webjars-play" % "2.5.0",
  "org.webjars" % "bootstrap" % "3.3.7-1",
  "org.webjars" % "font-awesome" % "4.7.0",
  "org.webjars" % "bootstrap-datepicker" % "1.6.4",
  "org.webjars.npm" % "arrive" % "2.3.1",
  "org.webjars" % "momentjs" % "2.18.1",
  "org.webjars" % "bootstrap-material-design" % "0.5.9",
  "org.webjars" % "jquery-validation" % "1.16.0",
  "org.webjars.bower" % "roboto-fontface" % "0.7.0",
  "org.apache.httpcomponents" % "httpclient" % "4.5.3",
  "org.apache.httpcomponents" % "httpcore" % "4.4.6",
  "jp.t2v" %% "play2-auth"      % "0.14.2",
  "jp.t2v" %% "play2-auth-test" % "0.14.2" % "test",
  "com.typesafe.play" %% "play-slick" % "2.1.0",
  "com.typesafe.play" %% "play-slick-evolutions" % "2.1.0",
  "org.mariadb.jdbc" % "mariadb-java-client" % "1.6.1",
  "commons-io" % "commons-io" % "2.5",
  "com.github.nscala-time" %% "nscala-time" % "2.16.0",
  "com.googlecode.htmlcompressor" % "htmlcompressor" % "1.5.2",
  "rhino" % "js" % "1.7R2",
  "com.typesafe.slick" %% "slick" % "3.2.0",
  "com.typesafe.slick" %% "slick-hikaricp" % "3.2.0",
  "com.vmunier" %% "scalajs-scripts" % "1.0.0",
  "com.zaxxer" % "HikariCP" % "2.6.1",
  "org.spire-math" %% "spire" % spireVersion,
  "org.spire-math" %% "spire-extras" % spireVersion,
  "com.google.guava" % "guava" % "22.0"
).map(_.exclude("com.zaxxer", "HikariCP-java6"))

resolvers ++= Seq(
  "SpinGo OSS" at "http://spingo-oss.s3.amazonaws.com/repositories/releases"
)

val opRabbitVersion = "2.0.0-rc1"

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

