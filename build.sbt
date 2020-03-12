import sbt._
import Keys._
import ReleaseTransformations._

lazy val coreSettings =
  commonSettings ++ scalacSettings ++ xlintOptions ++ scoverageSettings ++ releaseSettings

lazy val commonSettings = Seq(
  organization := "com.pavlosgi",
  scalaVersion := "2.12.10",
  crossScalaVersions := Seq("2.11.12", "2.12.10", "2.13.1"),
  offline := true,
  excludeFilter in unmanagedResources := NothingFilter,
  libraryDependencies ++=
    (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, scalaMajor)) if scalaMajor == 12 =>
        compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3") :: Nil
      case _ =>
        compilerPlugin("org.typelevel" % "kind-projector" % "0.11.0" cross CrossVersion.full) :: Nil
    }),
  resolvers := Seq(
    Resolver.sonatypeRepo("public"),
    Opts.resolver.sonatypeReleases,
    Opts.resolver.sonatypeSnapshots,
    Classpaths.sbtPluginReleases
  ),
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
        "-Xlint:package-object-classes",
        "-Xlint:stars-align",
        "-Xlint:constant")
//        "-Ymacro-annotations")
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
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Xfatal-warnings") ++
    (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, scalaMajor)) if scalaMajor >= 13 => Seq.empty
      case _ =>
        Seq("-Ywarn-unused-import",
          "-Ypartial-unification",
          "-Yno-adapted-args",
          "-Xfuture")
    })
)

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
  homepage := Some(url("https://github.com/pavlosgi/freecli")),
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
  .aggregate(freecli_testkit)
  .aggregate(freecli_circe)
  .aggregate(freecli_core)
  .aggregate(freecli_examples)

lazy val freecli_circe = Project("freecli-circe", file("circe"))
  .settings(coreSettings:_*)
  .settings(publishSettings:_*)
  .settings(libraryDependencies ++= circe(scalaVersion.value))
  .dependsOn(freecli_core)
  .dependsOn(freecli_testkit % Test)

lazy val freecli_core = Project("freecli-core", file("core"))
  .settings(coreSettings:_*)
  .settings(publishSettings:_*)
  .settings(libraryDependencies ++=
    cats(scalaVersion.value) ++
    shapeless(scalaVersion.value))

  .dependsOn(freecli_testkit % Test)

lazy val freecli_examples = Project("freecli-examples", file("examples"))
  .settings(coreSettings:_*)
  .settings(noPublishSettings:_*)
  .dependsOn(freecli_core)

lazy val freecli_testkit = Project("freecli-testkit", file("testkit"))
  .settings(coreSettings:_*)
  .settings(noPublishSettings:_*)
  .settings(libraryDependencies ++= cats(scalaVersion.value) ++ scalatest(scalaVersion.value))

def crossVersions(version: String) =
  CrossVersion.partialVersion(version) match {
    case Some((2, scalaMajor)) if scalaMajor >= 13 =>
      val catsV = "2.1.1"
      val circeV = "0.13.0"
      val shapelessV = "2.3.3"
      val scalatestV = "3.1.1"
      (catsV, circeV, shapelessV, scalatestV)

    case _ =>
      val catsV = "1.5.0"
      val circeV = "0.10.1"
      val shapelessV = "2.3.3"
      val scalatestV = "3.1.1"
      (catsV, circeV, shapelessV, scalatestV)
  }

def cats(scalaVersion: String) = Seq(
  "org.typelevel" %% "cats-core" % crossVersions(scalaVersion)._1,
  "org.typelevel" %% "cats-free" % crossVersions(scalaVersion)._1)

def circe(scalaVersion: String) = Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % crossVersions(scalaVersion)._2)

def shapeless(scalaVersion: String) = Seq("com.chuusai" %% "shapeless" % crossVersions(scalaVersion)._3)
def scalatest(scalaVersion: String) = Seq(
"org.scalactic" %% "scalactic" % crossVersions(scalaVersion)._4,
"org.scalatest" %% "scalatest" % crossVersions(scalaVersion)._4
)


addCommandAlias("root", ";project freecli-root")
addCommandAlias("core", ";project freecli-core")
addCommandAlias("examples", ";project freecli-examples")
addCommandAlias("circe", ";project freecli-circe")

addCommandAlias("validate", ";root;validateJVM")
addCommandAlias("validateJVM", ";freecli-core/compile;freecli-core/test;freecli-examples/compile;freecli-circe/compile;freecli-circe/test;freecli-testkit/compile")

addCommandAlias("releaseAll", ";root;release skip-tests")
