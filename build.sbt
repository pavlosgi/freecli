import sbt._
import Keys._
import ReleaseTransformations._

lazy val coreSettings =
  commonSettings ++ scalacSettings ++ xlintOptions ++ scoverageSettings ++ releaseSettings

lazy val commonSettings = Seq(
  organization := "com.pavlosgi",
  scalaVersion := "2.12.7",
  crossScalaVersions := Seq("2.11.12", "2.12.7"),
  offline := true,
  excludeFilter in unmanagedResources := NothingFilter,
  resolvers := Seq(
    Resolver.sonatypeRepo("public"),
    Opts.resolver.sonatypeReleases,
    Opts.resolver.sonatypeSnapshots,
    Classpaths.sbtPluginReleases
  ),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  scalacOptions in (Compile, console) := Seq.empty,
  scalacOptions in (Test, console) := Seq.empty,
  initialCommands in console := """""",
  unmanagedSourceDirectories in Compile += {
    (sourceDirectory in Compile).value / ("scala_" + scalaBinaryVersion.value)
  },
  unmanagedSourceDirectories in Test += {
    (sourceDirectory in Test).value / ("scala_" + scalaBinaryVersion.value)
  })
//  excludeFilter in Compile := "config" || "command" || "argument",
//  excludeFilter in Test := "config" || "command" || "argument")
lazy val xlintOptions = Seq(
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, scalaMajor)) if scalaMajor >= 12 =>
        Seq(
        "-Xlint:adapted-args",
        "-Xlint:nullary-unit",
        "-Xlint:inaccessible",
        "-Xlint:nullary-override",
        "-Xlint:infer-any",
        "-Xlint:missing-interpolator",
        "-Xlint:doc-detached",
        "-Xlint:private-shadow",
        "-Xlint:type-parameter-shadow",
        "-Xlint:poly-implicit-overload",
        "-Xlint:option-implicit",
        "-Xlint:delayedinit-select",
        "-Xlint:by-name-right-associative",
        "-Xlint:package-object-classes",
        "-Xlint:unsound-match",
        "-Xlint:stars-align",
        "-Xlint:constant")
      case _ => Seq("-Xlint")
    }
  })

lazy val scalacSettings = Seq(
  scalacOptions ++= Seq(
    "-unchecked",
    "-deprecation",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:postfixOps",
    "-language:experimental.macros",
    "-language:existentials",
    "-Ywarn-unused-import",
    "-Ypartial-unification",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Xfuture",
    "-Xfatal-warnings"))

lazy val scoverageSettings = Seq(
  coverageMinimum := 60,
  coverageFailOnMinimum := false,
  coverageExcludedFiles := ".*/src/test/.*",
  coverageExcludedPackages := "freecli.(examples.*|testkit.*)"
)

lazy val publishSettings = Seq(
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  homepage := Some(url("https://github.com/pavlosgi/freecli")),
  licenses := Seq("Apache 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := Function.const(false),
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/pavlosgi/freecli"),
      "scm:git:git@github.com:pavlosgi/freecli.git"
    )
  ),
  developers := List(
    Developer(
      "pavlosgi",
      "Pavlos Georgiou",
      "pavlos.georgiou.p@gmail.com",
      url("https://twitter.com/pavlosgi"))
  )
)


lazy val tagName = Def.setting{
  s"freecli-${if (releaseUseGlobalVersion.value) (version in ThisBuild).value else version.value}"
}

lazy val releaseSettings = Seq(
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  releaseTagName := tagName.value,
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    publishArtifacts,
    setNextVersion,
    commitNextVersion,
    ReleaseStep(action = Command.process("sonatypeReleaseAll", _)),
    pushChanges
  )
)

credentials in ThisBuild ++= (for {
  username <- Option(System.getenv().get("SONATYPE_USERNAME"))
  password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq

lazy val noPublishSettings = Seq(
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  publish := (),
  publishLocal := (),
  publishArtifact := false
)

lazy val root = Project("freecli-root", file("."))
  .settings(coreSettings:_*)
  .settings(noPublishSettings:_*)
  .aggregate(freecli_circe)
  .aggregate(freecli_core)
  .aggregate(freecli_examples)
  .aggregate(freecli_testkit)

lazy val freecli_circe = Project("freecli-circe", file("circe"))
  .settings(coreSettings:_*)
  .settings(publishSettings:_*)
  .settings(libraryDependencies ++= circe)
  .dependsOn(freecli_core)
  .dependsOn(freecli_testkit % Test)

lazy val freecli_core = Project("freecli-core", file("core"))
  .settings(coreSettings:_*)
  .settings(publishSettings:_*)
  .settings(libraryDependencies ++=
    cats ++
    shapeless)

  .dependsOn(freecli_testkit % Test)

lazy val freecli_examples = Project("freecli-examples", file("examples"))
  .settings(coreSettings:_*)
  .settings(noPublishSettings:_*)
  .dependsOn(freecli_core)

lazy val freecli_testkit = Project("freecli-testkit", file("testkit"))
  .settings(coreSettings:_*)
  .settings(noPublishSettings:_*)
  .settings(libraryDependencies ++= cats ++ scalatest)

lazy val catsV = "1.5.0"
lazy val circeV = "0.10.1"
lazy val shapelessV = "2.3.3"
lazy val scalatestV = "3.0.5"

lazy val cats = Seq(
  "org.typelevel" %% "cats-core" % catsV,
  "org.typelevel" %% "cats-free" % catsV)

lazy val circe = Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeV)

lazy val shapeless = Seq("com.chuusai" %% "shapeless" % shapelessV)
lazy val scalatest = Seq(
"org.scalactic" %% "scalactic" % scalatestV,
"org.scalatest" %% "scalatest" % scalatestV
)


addCommandAlias("root", ";project freecli-root")
addCommandAlias("core", ";project freecli-core")
addCommandAlias("examples", ";project freecli-examples")
addCommandAlias("circe", ";project freecli-circe")

addCommandAlias("validate", ";root;validateJVM")
addCommandAlias("validateJVM", ";freecli-core/compile;freecli-core/mimaReportBinaryIssues;freecli-core/test;freecli-examples/compile;freecli-circe/compile;freecli-circe/test;freecli-testkit/compile")

addCommandAlias("releaseAll", ";root;release skip-tests")
