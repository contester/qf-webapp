name := "qf"

scalaVersion  := "2.11.11"

organization := "org.stingray.contester"

version := "0.2"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-optimise", "-explaintypes", "-Xcheckinit",
  "-Xlint", "-feature")

resolvers ++= Seq(
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Atlassian Releases" at "https://maven.atlassian.com/public/",
  "SpinGo OSS" at "http://spingo-oss.s3.amazonaws.com/repositories/releases"
)

updateOptions := updateOptions.value.withCachedResolution(true)

val spireVersion = "0.13.0"

val silhouetteVersion = "4.0.0"

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
  "org.apache.httpcomponents" % "httpcore" % "4.4.7",
  "jp.t2v" %% "play2-auth"      % "0.14.2",
  "jp.t2v" %% "play2-auth-test" % "0.14.2" % "test",
  "com.typesafe.play" %% "play-slick" % "2.1.1",
  "com.typesafe.play" %% "play-slick-evolutions" % "2.1.1",
  "org.mariadb.jdbc" % "mariadb-java-client" % "2.0.3",
  "commons-io" % "commons-io" % "2.5",
  "com.github.nscala-time" %% "nscala-time" % "2.16.0",
  "com.googlecode.htmlcompressor" % "htmlcompressor" % "1.5.2",
  "rhino" % "js" % "1.7R2",
  "org.scala-lang.modules" %% "scala-async" % "0.9.6",
  "com.typesafe.slick" %% "slick" % "3.2.1",
  "com.typesafe.slick" %% "slick-hikaricp" % "3.2.1",
  "com.zaxxer" % "HikariCP" % "2.6.3",
  "org.typelevel" %% "cats" % "0.9.0",
//  "com.mohiva" %% "play-silhouette" % silhouetteVersion,
//  "com.mohiva" %% "play-silhouette-password-bcrypt" % silhouetteVersion,
//  "com.mohiva" %% "play-silhouette-crypto-jca" % silhouetteVersion,
//  "com.mohiva" %% "play-silhouette-persistence" % silhouetteVersion,
//  "com.mohiva" %% "play-silhouette-testkit" % silhouetteVersion % "test",
  "org.spire-math" %% "spire" % spireVersion,
  "org.spire-math" %% "spire-extras" % spireVersion,
  "com.google.guava" % "guava" % "23.0"
).map(_.exclude("com.zaxxer", "HikariCP-java6"))

val opRabbitVersion = "2.0.0"

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

