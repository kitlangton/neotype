inThisBuild(
  List(
    name           := "neotype",
    normalizedName := "neotype",
    organization   := "io.github.kitlangton",
    scalaVersion   := "3.2.2",
    homepage       := Some(url("https://github.com/kitlangton/neotype")),
    licenses       := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer("kitlangton", "Kit Langton", "kit.langton@gmail.com", url("https://github.com/kitlangton"))
    )
  )
)

Global / onChangedBuildSource := ReloadOnSourceChanges

val zioVersion = "2.0.9"

val sharedSettings = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
//    "-explain",
    "-Xcheck-macros"
//    "-Ycheck:all"
  ),
  libraryDependencies ++= Seq(
    "dev.zio" %% "zio-test"     % zioVersion % Test,
    "dev.zio" %% "zio-test-sbt" % zioVersion % Test
  )
)

lazy val root = (project in file("."))
  .settings(
    name := "neotype"
  )
  .aggregate(
    core,
    zioJson,
    zioQuill,
    examples,
    tapir
  )

lazy val core = (project in file("modules/core"))
  .settings(
    name := "neotype",
    sharedSettings
  )

lazy val zioJson = (project in file("modules/neotype-zio-json"))
  .settings(
    name := "neotype-zio-json",
    sharedSettings,
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-json" % "0.4.2"
    )
  )
  .dependsOn(core)

lazy val zioQuill = (project in file("modules/neotype-zio-quill"))
  .settings(
    name := "neotype-zio-quill",
    sharedSettings,
    libraryDependencies ++= Seq(
      "io.getquill"   %% "quill-jdbc-zio" % "4.6.0.1",
      "org.postgresql" % "postgresql"     % "42.5.4"  % Test,
      "com.h2database" % "h2"             % "2.1.214" % Test
    )
  )
  .dependsOn(core)

lazy val zio = (project in file("modules/neotype-zio"))
  .settings(
    name := "neotype-zio",
    sharedSettings,
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % zioVersion
    )
  )
  .dependsOn(core)

lazy val tapir = (project in file("modules/tapir"))
  .settings(
    name := "neotypes-tapir",
    sharedSettings,
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.tapir" %% "tapir-core" % "1.2.9",
    )
  )
  .dependsOn(core)

lazy val examples = (project in file("examples"))
  .settings(
    name := "neotype-examples",
    sharedSettings,
    publish / skip := true
  )
  .dependsOn(core, zioJson, zioQuill)
