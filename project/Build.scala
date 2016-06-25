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
    scalaVersion := "2.11.8",
    offline := true,
    excludeFilter in unmanagedResources := NothingFilter,
    resolvers := Seq(
      Resolver.sonatypeRepo("public"),
      Opts.resolver.sonatypeReleases,
      Opts.resolver.sonatypeSnapshots,
      Classpaths.sbtPluginReleases
    ),
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1"),
    addCompilerPlugin("com.milessabin" % "si2712fix-plugin_2.11.8" % "1.1.0"),
    scalacOptions in (Compile, console) := Seq.empty,
    scalacOptions in (Test, console) := Seq.empty,
    initialCommands in console := """"""
  )

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
      "-Xfatal-warnings")
  )

  val allSettings = commonSettings ++
                    scalacSettings

  lazy val root = Project("freecli-root", file("."))
    .settings(allSettings:_*)
    .aggregate(freecli_circe)
    .aggregate(freecli_core)
    .aggregate(freecli_examples)

  lazy val freecli_circe = Project("freecli-circe", file("circe"))
    .settings(allSettings:_*)
    .settings(libraryDependencies ++= Dependencies.circe ++ Dependencies.scalatest)
    .dependsOn(freecli_core)

  lazy val freecli_core = Project("freecli-core", file("core"))
    .settings(allSettings:_*)
    .settings(libraryDependencies ++=
      Dependencies.cats ++
      Dependencies.kittens ++
      Dependencies.scalatest ++
      Dependencies.shapeless)

  lazy val freecli_examples = Project("freecli-examples", file("examples"))
    .settings(allSettings:_*)
    .dependsOn(freecli_core)
}

object Dependencies {
  val shapeless = Seq("com.chuusai" %% "shapeless" % "2.3.1")

  val scalatest = Seq(
    "org.scalactic" %% "scalactic" % "3.0.0-M15",
    "org.scalatest" % "scalatest_2.11" % "3.0.0-M15" % "test"
  )

  val catsVersion = "0.5.0"
  val cats = Seq("org.typelevel" %% "cats" % catsVersion)

  val circeVersion = "0.4.1"

  val circe = Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
    "io.circe" %% "circe-parser"
  ).map(_ % circeVersion)

  val effCats = Seq("org.atnos" %% "eff-cats" % "1.7-20160526110310-1704d42")

  val commonsIO = Seq("commons-io" % "commons-io" % "2.5")
  val config = Seq("com.typesafe" % "config" % "1.3.0")

  val nscalaTime = Seq("com.github.nscala-time" %% "nscala-time" % "2.12.0")
  
  val kittens = Seq("org.typelevel" %% "kittens" %"1.0.0-M3")

}
