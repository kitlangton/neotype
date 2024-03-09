inThisBuild(
  List(
    name           := "neotype",
    normalizedName := "neotype",
    organization   := "io.github.kitlangton",
    scalaVersion   := "3.3.2",
    homepage       := Some(url("https://github.com/kitlangton/neotype")),
    licenses       := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer("kitlangton", "Kit Langton", "kit.langton@gmail.com", url("https://github.com/kitlangton"))
    )
  )
)

Global / onChangedBuildSource := ReloadOnSourceChanges

val tapirVersion = "1.9.10"
val zioVersion   = "2.0.21"

val sharedSettings = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
//    "-explain",
    "-Xcheck-macros",
    //    "-Ycheck:all",
    "-Wunused:all"
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
    circe.js,
    circe.jvm,
    core.js,
    core.jvm,
    jsoniter.js,
    jsoniter.jvm,
    examples,
    tapir.js,
    tapir.jvm,
    zioConfig,
    zioJson.js,
    zioJson.jvm,
    zioQuill,
    zioSchema.js,
    zioSchema.jvm,
    zioTest.js,
    zioTest.jvm
  )

lazy val core = (crossProject(JSPlatform, JVMPlatform) in file("modules/core"))
  .settings(
    name := "neotype",
    sharedSettings
  )

lazy val circe = (crossProject(JSPlatform, JVMPlatform) in file("modules/neotype-circe"))
  .settings(
    name := "neotype-circe",
    sharedSettings,
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core"   % "0.14.6",
      "io.circe" %%% "circe-parser" % "0.14.6"
    )
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val jsoniter = (crossProject(JSPlatform, JVMPlatform) in file("modules/neotype-jsoniter"))
  .settings(
    name := "neotype-jsoniter",
    sharedSettings,
    libraryDependencies ++= Seq(
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core"   % "2.28.2",
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-macros" % "2.28.2"
    )
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val tapir = (crossProject(JSPlatform, JVMPlatform) in file("modules/neotype-tapir"))
  .settings(
    name := "neotype-tapir",
    sharedSettings,
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.tapir" %%% "tapir-core"         % tapirVersion,
      "com.softwaremill.sttp.tapir" %%% "tapir-json-pickler" % tapirVersion
    )
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val zioConfig = (project in file("modules/neotype-zio-config"))
  .settings(
    name := "neotype-zio-config",
    sharedSettings,
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-config"          % "4.0.1",
      "dev.zio" %% "zio-config-magnolia" % "4.0.1"
    )
  )
  .dependsOn(core.jvm % "compile->compile;test->test")

lazy val zioSchema = (crossProject(JSPlatform, JVMPlatform) in file("modules/neotype-zio-schema"))
  .settings(
    name := "neotype-zio-schema",
    sharedSettings,
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio-schema"      % "1.0.1",
      "dev.zio" %%% "zio-json"        % "0.6.2" % Test,
      "dev.zio" %%% "zio-schema-json" % "1.0.1" % Test
    )
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val zioJson = (crossProject(JSPlatform, JVMPlatform) in file("modules/neotype-zio-json"))
  .settings(
    name := "neotype-zio-json",
    sharedSettings,
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio-json" % "0.6.2"
    )
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val zioQuill = (project in file("modules/neotype-zio-quill"))
  .settings(
    name := "neotype-zio-quill",
    sharedSettings,
    libraryDependencies ++= Seq(
      "io.getquill"   %% "quill-jdbc-zio" % "4.8.1",
      "org.postgresql" % "postgresql"     % "42.5.4"  % Test,
      "com.h2database" % "h2"             % "2.1.214" % Test
    )
  )
  .dependsOn(core.jvm % "compile->compile;test->test")

lazy val zioTest = (crossProject(JSPlatform, JVMPlatform) in file("modules/neotype-zio-test"))
  .settings(
    name := "neotype-zio-test",
    sharedSettings,
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio-test"          % zioVersion,
      "dev.zio" %%% "zio-test-magnolia" % zioVersion
    )
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val examples = (project in file("examples"))
  .settings(
    name := "neotype-examples",
    sharedSettings,
    publish / skip := true
  )
  .dependsOn(core.jvm, zioJson.jvm, zioQuill)

addCommandAlias("fmt", "scalafmtAll")
