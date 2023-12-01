inThisBuild(
  List(
    name           := "neotype",
    normalizedName := "neotype",
    organization   := "io.github.kitlangton",
    scalaVersion   := "3.3.1",
    homepage       := Some(url("https://github.com/kitlangton/neotype")),
    licenses       := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer("kitlangton", "Kit Langton", "kit.langton@gmail.com", url("https://github.com/kitlangton"))
    )
  )
)

Global / onChangedBuildSource := ReloadOnSourceChanges

val zioVersion = "2.0.19"

val sharedSettings = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
    //    "-explain",
    "-Xcheck-macros"
    //    "-Ycheck:all"
  ),
  libraryDependencies ++= Seq(
    "dev.zio" %%% "zio-test"     % zioVersion % Test,
    "dev.zio" %%% "zio-test-sbt" % zioVersion % Test
  )
)

lazy val root = (project in file("."))
  .settings(
    name := "neotype"
  )
  .aggregate(
    core.js,
    core.jvm,
    circe.js,
    circe.jvm,
    zio.js,
    zio.jvm,
    zioJson.js,
    zioJson.jvm,
    zioConfig,
    zioQuill,
    zioSchema.js,
    zioSchema.jvm,
    examples,
    tapir.js,
    tapir.jvm
  )

lazy val core = (crossProject(JSPlatform, JVMPlatform) in file("modules/core"))
  .settings(
    name := "neotype",
    sharedSettings
  )

lazy val zioJson = (crossProject(JSPlatform, JVMPlatform) in file("modules/neotype-zio-json"))
  .settings(
    name := "neotype-zio-json",
    sharedSettings,
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio-json" % "0.6.2"
    )
  )
  .dependsOn(core)

lazy val circe = (crossProject(JSPlatform, JVMPlatform) in file("modules/neotype-circe"))
  .settings(
    name := "neotype-circe",
    sharedSettings,
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core"   % "0.14.6",
      "io.circe" %%% "circe-parser" % "0.14.6"
    )
  )
  .dependsOn(core)

lazy val zioQuill = (project in file("modules/neotype-zio-quill"))
  .settings(
    name := "neotype-zio-quill",
    sharedSettings,
    libraryDependencies ++= Seq(
      "io.getquill"   %% "quill-jdbc-zio" % "4.8.0",
      "org.postgresql" % "postgresql"     % "42.5.4"  % Test,
      "com.h2database" % "h2"             % "2.1.214" % Test
    )
  )
  .dependsOn(core.jvm)

lazy val zio = (crossProject(JSPlatform, JVMPlatform) in file("modules/neotype-zio"))
  .settings(
    name := "neotype-zio",
    sharedSettings,
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio" % zioVersion
    )
  )
  .dependsOn(core)

lazy val zioConfig = (project in file("modules/neotype-zio-config"))
  .settings(
    name := "neotype-zio-config",
    sharedSettings,
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-config"          % "3.0.7",
      "dev.zio" %% "zio-config-magnolia" % "3.0.7"
    )
  )
  .dependsOn(core.jvm)

lazy val zioSchema = (crossProject(JSPlatform, JVMPlatform) in file("modules/neotype-zio-schema"))
  .settings(
    name := "neotype-zio-schema",
    sharedSettings,
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio-schema"      % "0.4.15",
      "dev.zio" %%% "zio-json"        % "0.6.2"  % Test,
      "dev.zio" %%% "zio-schema-json" % "0.4.15" % Test
    )
  )
  .dependsOn(core)

val tapirVersion = "1.9.0"
lazy val tapir = (crossProject(JSPlatform, JVMPlatform) in file("modules/neotype-tapir"))
  .settings(
    name := "neotype-tapir",
    sharedSettings,
    libraryDependencies ++= Seq(
      // TODO Does this make sense?
      "com.softwaremill.sttp.tapir" %%% "tapir-core"         % tapirVersion,
      "com.softwaremill.sttp.tapir" %%% "tapir-json-pickler" % tapirVersion
    )
  )
  .dependsOn(core)

lazy val examples = (project in file("examples"))
  .settings(
    name := "neotype-examples",
    sharedSettings,
    publish / skip := true
  )
  .dependsOn(core.jvm, zioJson.jvm, zioQuill)

addCommandAlias("fmt", "scalafmtAll")
