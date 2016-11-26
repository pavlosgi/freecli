import sbt._
import Keys._
import com.typesafe.sbt.packager.archetypes.JavaAppPackaging
import com.typesafe.sbt.SbtNativePackager._
import com.typesafe.sbt.packager.MappingsHelper._
import com.typesafe.sbt.packager.Keys._

object Build extends Build {

  val appVersion = "0.1.0-SNAPSHOT"

  val commonSettings = Seq(
    organization := "pavlosgi",
    version := appVersion,
    scalaVersion := "2.12.0",
    offline := true,
    excludeFilter in unmanagedResources := NothingFilter,
    resolvers := Seq(
      Resolver.sonatypeRepo("public"),
      Opts.resolver.sonatypeReleases,
      Opts.resolver.sonatypeSnapshots,
      Classpaths.sbtPluginReleases
    ),
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
    addCompilerPlugin("com.milessabin" % "si2712fix-plugin_2.11.8" % "1.2.0"),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
    scalacOptions in (Compile, console) := Seq.empty,
    scalacOptions in (Test, console) := Seq.empty,
    initialCommands in console := """""")

  val scalacSettings = Seq(
    scalacOptions ++= Seq(
      "-feature",
      "-unchecked",
      "-deprecation",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-language:postfixOps",
      "-language:experimental.macros",
      "-language:existentials",
      "-Ywarn-unused-import",
//      "-Ylog-classpath",
//      "-Ypartial-unification",
//      "-Xprint:typer",
      "-Xlint",
      "-Yno-adapted-args",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard",
      "-Xfuture",
      "-Xfatal-warnings")
  )

  val allSettings = commonSettings ++ scalacSettings

  lazy val root = Project("freecli-root", file("."))
    .settings(allSettings:_*)
    .aggregate(freecli_circe)
    .aggregate(freecli_core)
    .aggregate(freecli_examples)
    .aggregate(freecli_testkit)

  lazy val freecli_circe = Project("freecli-circe", file("circe"))
    .settings(allSettings:_*)
    .settings(libraryDependencies ++= Dependencies.circe)
    .dependsOn(freecli_core)
    .dependsOn(freecli_testkit % Test)

  lazy val freecli_core = Project("freecli-core", file("core"))
    .settings(allSettings:_*)
    .settings(libraryDependencies ++=
      Dependencies.cats ++
      Dependencies.shapeless)

    .dependsOn(freecli_testkit % Test)

  lazy val freecli_examples = Project("freecli-examples", file("examples"))
    .settings(allSettings:_*)
    .dependsOn(freecli_core)

  lazy val freecli_testkit = Project("freecli-testkit", file("testkit"))
    .settings(allSettings:_*)
    .settings(libraryDependencies ++=
      Dependencies.cats ++
      Dependencies.scalatest)
}

object Dependencies {
  val shapeless = Seq("com.chuusai" % "shapeless_2.12" % "2.3.2")

  val scalatest = Seq(
    "org.scalactic" %% "scalactic" % "3.0.0",
    "org.scalatest" %% "scalatest" % "3.0.0"
  )

  val catsVersion = "0.8.1"
  val cats = Seq("org.typelevel" %% "cats" % catsVersion)

  val circeVersion = "0.6.0"

  val circe = Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
    "io.circe" %% "circe-parser"
  ).map(_ % circeVersion)
}
