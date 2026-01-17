inThisBuild(
  List(
    organization  := "io.github.kitlangton",
    scalaVersion  := "3.3.7",
    versionScheme := Some("early-semver"),
    homepage      := Some(url("https://github.com/kitlangton/neotype")),
    licenses      := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer("kitlangton", "Kit Langton", "kit.langton@gmail.com", url("https://github.com/kitlangton"))
    ),
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision
  )
)

Global / onChangedBuildSource := ReloadOnSourceChanges

// CI memory optimization: disable parallel execution (fork only works for JVM, not JS/Native)
// Only apply in CI to keep local dev fast
val isCI = sys.env.get("CI").contains("true")
ThisBuild / Test / parallelExecution := !isCI

////////////////////////
// sbt-github-actions //
////////////////////////
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"))

ThisBuild / githubWorkflowEnv := Map("JAVA_OPTS" -> "-Xmx8g")

// Only test JVM in CI to reduce memory pressure (JS/Native tested locally)
ThisBuild / githubWorkflowBuild := Seq(
  WorkflowStep.Sbt(
    commands = List("compile", "Test/compile"),
    name = Some("Compile all")
  ),
  WorkflowStep.Sbt(
    commands = List("coreJVM/test", "comptimeJVM/test"),
    name = Some("Test JVM core modules")
  )
)

ThisBuild / githubWorkflowTargetTags ++= Seq("v*")
ThisBuild / githubWorkflowPublishTargetBranches :=
  Seq(
    RefPredicate.StartsWith(Ref.Tag("v"))
  )

ThisBuild / githubWorkflowPermissions := Some(Permissions.WriteAll)

ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Sbt(
    commands = List("ci-release"),
    name = Some("Publish project"),
    env = Map(
      "PGP_PASSPHRASE"    -> "${{ secrets.PGP_PASSPHRASE }}",
      "PGP_SECRET"        -> "${{ secrets.PGP_SECRET }}",
      "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
      "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}"
    )
  ),
  WorkflowStep.Run(
    commands = List("gh release create ${{ github.ref_name }} --generate-notes || true"),
    name = Some("Create GitHub Release"),
    env = Map("GH_TOKEN" -> "${{ secrets.GITHUB_TOKEN }}")
  )
)

/////////////////////////
// Project Definitions //
/////////////////////////

lazy val jsoniterVersion       = "2.38.8"
lazy val circeVersion          = "0.14.15"
lazy val tapirVersion          = "1.13.4"
lazy val zioVersion            = "2.1.24"
lazy val zioConfigVersion      = "4.0.6"
lazy val zioSchemaVersion      = "1.7.6"
lazy val zioJsonVersion        = "0.8.0"
lazy val chimneyVersion        = "1.8.2"
lazy val calibanVersion        = "2.11.2"
lazy val doobieVersion         = "1.0.0-RC11"
lazy val upickleVersion        = "4.4.2"
lazy val cirisVersion          = "3.12.0"
lazy val zioInteropCatsVersion = "23.1.0.13"
lazy val pureconfigVersion     = "0.17.9"
lazy val scanamoVersion        = "6.0.0"
lazy val tethysVersion         = "0.29.7"
lazy val catsVersion           = "2.13.0"

val sharedSettings = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
    "-Xcheck-macros",
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
    // JVM
    caliban.jvm,
    cats.jvm,
    chimney.jvm,
    circe.jvm,
    ciris.jvm,
    comptime.jvm,
    core.jvm,
    doobie.jvm,
    internal.jvm,
    jsoniter.jvm,
    playJson.jvm,
    pureconfig.jvm,
    scanamo.jvm,
    tapir.jvm,
    tethys.jvm,
    upickle.jvm,
    zioConfig,
    zioJson.jvm,
    zioQuill,
    zioSchema.jvm,
    zioTest.jvm,
    // JS
    cats.js,
    chimney.js,
    circe.js,
    ciris.js,
    comptime.js,
    core.js,
    internal.js,
    jsoniter.js,
    playJson.js,
    tapir.js,
    upickle.js,
    zioJson.js,
    zioSchema.js,
    zioTest.js,
    // Native (use `testNative` to run tests)
    cats.native,
    chimney.native,
    circe.native,
    comptime.native,
    core.native,
    internal.native,
    upickle.native,
    zioJson.native,
    zioSchema.native,
    // Other
    examples
  )

// Skip Native tests by default (use `testNative` to run them, or CI)
lazy val skipNativeTests = Seq(
  (Test / test) := {
    if (sys.env.contains("CI")) (Test / test).value
    else ()
  }
)

lazy val internal = (crossProject(JSPlatform, JVMPlatform, NativePlatform) in file("modules/internal"))
  .settings(
    name := "neotype-internal",
    sharedSettings
  )
  .nativeSettings(skipNativeTests)

lazy val core = (crossProject(JSPlatform, JVMPlatform, NativePlatform) in file("modules/core"))
  .settings(
    name := "neotype",
    sharedSettings
  )
  .nativeSettings(skipNativeTests)
  .dependsOn(
    internal % "compile->compile;test->test",
    comptime % "compile->compile;test->test"
  )

lazy val comptime = (crossProject(JSPlatform, JVMPlatform, NativePlatform) in file("modules/comptime"))
  .settings(
    name := "comptime",
    sharedSettings
  )
  .nativeSettings(skipNativeTests)
  .dependsOn(internal % "compile->compile;test->test")

lazy val circe = (crossProject(JSPlatform, JVMPlatform, NativePlatform) in file("modules/neotype-circe"))
  .settings(
    name := "neotype-circe",
    sharedSettings,
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core"   % circeVersion,
      "io.circe" %%% "circe-parser" % circeVersion
    )
  )
  .nativeSettings(skipNativeTests)
  .dependsOn(core % "compile->compile;test->test")

lazy val jsoniter = (crossProject(JSPlatform, JVMPlatform) in file("modules/neotype-jsoniter"))
  .settings(
    name := "neotype-jsoniter",
    sharedSettings,
    libraryDependencies ++= Seq(
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core"   % jsoniterVersion,
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-macros" % jsoniterVersion
    )
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val playJson = (crossProject(JSPlatform, JVMPlatform) in file("modules/neotype-play-json"))
  .settings(
    name := "neotype-play-json",
    sharedSettings,
    libraryDependencies ++= Seq(
      "org.playframework" %%% "play-json" % "3.1.0-M9"
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
      "dev.zio" %% "zio-config"          % zioConfigVersion,
      "dev.zio" %% "zio-config-magnolia" % zioConfigVersion
    )
  )
  .dependsOn(core.jvm % "compile->compile;test->test")

lazy val zioSchema = (crossProject(JSPlatform, JVMPlatform, NativePlatform) in file("modules/neotype-zio-schema"))
  .settings(
    name := "neotype-zio-schema",
    sharedSettings,
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio-schema"      % zioSchemaVersion,
      "dev.zio" %%% "zio-schema-json" % zioSchemaVersion % Test
    )
  )
  .nativeSettings(skipNativeTests)
  .dependsOn(core % "compile->compile;test->test")

lazy val zioJson = (crossProject(JSPlatform, JVMPlatform, NativePlatform) in file("modules/neotype-zio-json"))
  .settings(
    name := "neotype-zio-json",
    sharedSettings,
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio-json" % zioJsonVersion
    )
  )
  .nativeSettings(skipNativeTests)
  .dependsOn(core % "compile->compile;test->test")

lazy val zioQuill = (project in file("modules/neotype-zio-quill"))
  .settings(
    name := "neotype-zio-quill",
    sharedSettings,
    libraryDependencies ++= Seq(
      "io.getquill"   %% "quill-jdbc-zio" % "4.8.6",
      "org.postgresql" % "postgresql"     % "42.7.9"  % Test,
      "com.h2database" % "h2"             % "2.4.240" % Test
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

lazy val chimney = (crossProject(JSPlatform, JVMPlatform, NativePlatform) in file("modules/neotype-chimney"))
  .settings(
    name := "neotype-chimney",
    sharedSettings,
    libraryDependencies ++= Seq("io.scalaland" %%% "chimney" % chimneyVersion)
  )
  .nativeSettings(skipNativeTests)
  .dependsOn(core % "compile->compile;test->test")

lazy val caliban = (crossProject(JVMPlatform) in file("modules/neotype-caliban"))
  .settings(
    name := "neotype-caliban",
    sharedSettings,
    libraryDependencies ++= Seq(
      "com.github.ghostdogpr" %% "caliban" % calibanVersion
    )
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val doobie = (crossProject(JVMPlatform) in file("modules/neotype-doobie"))
  .settings(
    name := "neotype-doobie",
    sharedSettings,
    libraryDependencies ++= Seq(
      "org.tpolecat"  %% "doobie-core"     % doobieVersion,
      "org.tpolecat"  %% "doobie-postgres" % doobieVersion % Test,
      "com.h2database" % "h2"              % "2.4.240"     % Test
    )
  )
  .dependsOn(core % "compile->compile;test->test", cats % "compile->compile;test->test")

lazy val upickle = (crossProject(JSPlatform, JVMPlatform, NativePlatform) in file("modules/neotype-upickle"))
  .settings(
    name := "neotype-upickle",
    sharedSettings,
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "upickle" % upickleVersion
    )
  )
  .nativeSettings(skipNativeTests)
  .dependsOn(core % "compile->compile;test->test")

lazy val ciris = (crossProject(JSPlatform, JVMPlatform) in file("modules/neotype-ciris"))
  .settings(
    name := "neotype-ciris",
    sharedSettings,
    libraryDependencies ++= Seq(
      "is.cir"  %%% "ciris"            % cirisVersion,
      "dev.zio" %%% "zio-interop-cats" % zioInteropCatsVersion % Test
    )
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val pureconfig = (crossProject(JVMPlatform) in file("modules/neotype-pureconfig"))
  .settings(
    name := "neotype-pureconfig",
    sharedSettings,
    libraryDependencies ++= Seq(
      "com.github.pureconfig" %% "pureconfig-core"         % pureconfigVersion,
      "com.github.pureconfig" %% "pureconfig-generic-base" % pureconfigVersion
    )
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val tethys = (crossProject(JVMPlatform) in file("modules/neotype-tethys"))
  .settings(
    name := "neotype-tethys",
    sharedSettings,
    libraryDependencies ++= Seq(
      "com.tethys-json" %% "tethys-core"       % tethysVersion,
      "com.tethys-json" %% "tethys-jackson213" % tethysVersion
    )
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val scanamo = (crossProject(JVMPlatform) in file("modules/neotype-scanamo"))
  .settings(
    name := "neotype-scanamo",
    sharedSettings,
    libraryDependencies ++= Seq(
      "org.scanamo" %% "scanamo" % scanamoVersion
    )
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val cats = (crossProject(JVMPlatform, JSPlatform, NativePlatform) in file("modules/neotype-cats"))
  .settings(
    name := "neotype-cats",
    sharedSettings,
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % catsVersion
    )
  )
  .nativeSettings(skipNativeTests)
  .dependsOn(core % "compile->compile;test->test")

lazy val examples = (project in file("examples"))
  .settings(
    name := "neotype-examples",
    sharedSettings,
    publish / skip := true,
    // Allow zio-json version mismatch (quill uses older version)
    evictionErrorLevel := Level.Warn
  )
  .dependsOn(core.jvm, comptime.jvm, zioJson.jvm, zioQuill)

addCommandAlias("prepare", "scalafixAll;scalafmtAll;githubWorkflowGenerate")
addCommandAlias(
  "testNative",
  List(
    "coreNative/test",
    "circeNative/test",
    "zioJsonNative/test",
    "zioSchemaNative/test",
    "chimneyNative/test",
    "upickleNative/test",
    "catsNative/test"
  ).mkString(";")
)

welcomeMessage

def welcomeMessage =
  onLoadMessage := {
    import scala.Console

    def header(text: String): String = s"${Console.RED}$text${Console.RESET}"
    def item(text: String): String   = s"${Console.GREEN}> ${Console.CYAN}$text${Console.RESET}"

    s"""|${header(" _   _ _____ _____ _______   _______ _____ ")}
        |${header("| \\ | |  ___|  _  |_   _\\ \\ / | ___ |  ___|")}
        |${header("|  \\| | |__ | | | | | |  \\ V /| |_/ | |__ ")}
        |${header("| . ` |  __|| | | | | |   \\ / |  __/|  __|")}
        |${header("| |\\  | |___\\ \\_/ / | |   | | | |   | |___")}
        |${header("\\_| \\_\\____/ \\___/  \\_/   \\_/ \\_|   \\____/")}
        |${header("——————————————————————————————————————————")}
        |${header("———————— NEWTYPES + REFINED TYPES ————————")}
        |${header("——————————————————————————————————————————")}

        |Useful sbt tasks:

        |${item("prepare")} - Runs scalafix and scalafmt on all files
        |${item("~compile")} - Compiles all modules (file-watch enabled)
        |${item("test")} - Runs all tests

    """.stripMargin

  }
