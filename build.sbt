name := "simplejsonparser"

version := "0.1"

scalaVersion := "2.13.4"


inThisBuild(Seq(
  version := "0.1",
  isSnapshot := true,
  scalaVersion := "2.12.10",
  autoStartServer := false
))

lazy val bspScalacOptions = Seq(
  "-feature",
  "-deprecation",
  "-encoding", "UTF-8",
  "-unchecked",
  "-deprecation",
  "-Xfuture",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-language:postfixOps",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-Ywarn-value-discard",
  "-Ypartial-unification"
)


lazy val `cats-version` = "2.2.0"
lazy val cats = "org.typelevel" %% "cats-core" % `cats-version`
lazy val kittens = "org.typelevel" %% "kittens" % `cats-version`
lazy val `zio-version` = "1.0.3"
lazy val zio = "dev.zio" %% "zio" %  `zio-version`
lazy val `zio-streams` = "dev.zio" %% "zio-streams" %  `zio-version`
lazy val `zio-test` = "dev.zio" %% "zio-test" % `zio-version` % "test"
lazy val `zio-test-sbt` = "dev.zio" %% "zio-test-sbt" % `zio-version` % "test"

lazy val commonSettings = Seq(
  parallelExecution in Test := false,
  scalacOptions ++= bspScalacOptions,
  organization := "tewecske",
  testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.10"),
  exportJars := true,
  updateOptions := updateOptions.value.withCachedResolution(true),
  libraryDependencies ++= Seq(
    zio,
    `zio-streams`,
    cats,
    kittens,
    `zio-test`,
    `zio-test-sbt`
  )
)

lazy val `simplejsonparser-main` = (project in file ("simplejsonparser-main"))
  .settings(commonSettings: _*)

