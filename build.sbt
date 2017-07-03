enablePlugins(AutomateHeaderPlugin)
headerLicense := Some(HeaderLicense.ALv2("2017", "Nikolay Donets"))

lazy val buildSettings = Seq(
  organization := "com.github.nikdon",
  name := "scala-cloud-datastore",
  version := "0.1.0",
  scalaVersion := "2.12.2",
  licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))
)

lazy val compilerOptions = Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-unchecked",
  "-Xfatal-warnings",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Xfuture"
)

lazy val baseSettings = Seq(
  scalacOptions ++= compilerOptions ++ Seq(
    "-Ywarn-unused-import"
  ),
  scalacOptions in (Compile, console) := compilerOptions,
  scalacOptions in (Compile, test) := compilerOptions,
  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  ),
  mappings.in(Compile, packageBin) += baseDirectory
    .in(ThisBuild)
    .value / "LICENSE" -> "LICENSE"
)

val catsV                 = "0.9.0"
val googleCloudDatastoreV = "1.1.0"
val shapelessV            = "2.3.2"
val scalaTestV            = "3.0.3"
val scalaCheckV           = "1.13.5"
val scalaCheckShapelessV  = "1.1.5"

lazy val root = (project in file("."))
  .settings(buildSettings)
  .settings(baseSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel"              %% "cats"                      % catsV,
      "com.google.cloud"           % "google-cloud-datastore"     % googleCloudDatastoreV,
      "com.chuusai"                %% "shapeless"                 % shapelessV,
      "org.scalatest"              %% "scalatest"                 % scalaTestV % "test",
      "org.scalacheck"             %% "scalacheck"                % scalaCheckV % "test",
      "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % scalaCheckShapelessV % "test"
    ),
    fork := true
  )
